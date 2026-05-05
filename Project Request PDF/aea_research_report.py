import argparse
from datetime import datetime
import http.cookiejar as cookiejar
import io
import re
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable
from urllib.parse import parse_qs, unquote, urlparse

import pandas as pd
import requests
from xml.sax.saxutils import escape as xml_escape

try:
    from reportlab.lib import colors
    from reportlab.lib.enums import TA_CENTER, TA_LEFT
    from reportlab.lib.pagesizes import letter, landscape
    from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
    from reportlab.lib.units import inch
    from reportlab.pdfbase import pdfmetrics
    from reportlab.pdfbase.pdfmetrics import stringWidth
    from reportlab.pdfbase.ttfonts import TTFont
    from reportlab.pdfgen import canvas as rl_canvas
    from reportlab.platypus import Paragraph, SimpleDocTemplate, Spacer, Table, TableStyle
except Exception as exc:  # pragma: no cover - handled in runtime usage
    raise RuntimeError(
        "reportlab is required. Install with: python -m pip install reportlab"
    ) from exc

try:
    from pypdf import PdfReader, PdfWriter
    HAVE_PYPDF = True
except Exception:
    HAVE_PYPDF = False


PROJECT_PROTOCOL_COL = (
    "Upload your Project Protocol file. Please use the following naming convention "
    "(PILastName_ProjectName_FieldName_Protocols.pdf)"
)
PLOT_MAP_COL = "Upload Plot Map"
PLOT_MAP_FLAG_COL = "Do you have a plot map to upload?"


REQUIRED_COLS = [
    "ReportingYear",
    "Project Title",
    "Email",
    "Name",
    PLOT_MAP_FLAG_COL,
    "Enter your land needs.",
    "Will you install sensors in the field?",
    "Faculty PI Last Name",
    "Faculty PI Email address",
    "Faculty PI Phone number",
    "Project Contact Name",
    "Project Contact Email address",
    "Project Contact Phone number",
    PROJECT_PROTOCOL_COL,
]


LABEL_MAP = [
    ("Email", "Email"),
    ("Name", "Name"),
    (PLOT_MAP_FLAG_COL, "Do you have a plot map to upload?"),
    ("Enter your land needs.", "Enter your land needs"),
    ("Will you install sensors in the field?", "Will you install sensors in the field?"),
    ("Faculty PI Last Name", "Faculty PI last name"),
    ("Faculty PI Email address", "Faculty PI email address"),
    ("Faculty PI Phone number", "Faculty PI phone number"),
    ("Project Contact Name", "Project contact name"),
    ("Project Contact Email address", "Project contact email"),
    ("Project Contact Phone number", "Project contact phone"),
]


INVALID_FILENAME_CHARS = re.compile(r'[<>:"/\\|?*]')
NON_ALNUM = re.compile(r"[^a-z0-9]")


@dataclass
class DownloadResult:
    ok: bool
    path: Path | None
    message: str
    url: str | None = None
    is_pdf: bool = False


@dataclass
class ConversionResult:
    ok: bool
    path: Path | None
    message: str


def _safe_str(value) -> str:
    if value is None:
        return ""
    if pd.isna(value):
        return ""
    return str(value).strip()


def _is_yes(value) -> bool:
    return _safe_str(value).lower() in {"yes", "y", "true", "1"}


def _sanitize_filename(text: str, fallback: str = "file") -> str:
    text = _safe_str(text) or fallback
    text = INVALID_FILENAME_CHARS.sub("_", text)
    text = re.sub(r"\s+", "_", text).strip("_")
    return text or fallback


def _normalize_name(text: str) -> str:
    text = _safe_str(text).lower()
    text = re.sub(r"\s+", "", text)
    return NON_ALNUM.sub("", text)


def _pick_field_name(row: pd.Series) -> str:
    for col in ("Field Name", "Field", "FieldName"):
        if col in row.index:
            val = _safe_str(row.get(col))
            if val:
                return val
    return "UnknownField"


def _extract_filename_from_url(url: str) -> str:
    if not url:
        return "attachment"
    parsed = urlparse(url)
    qs = parse_qs(parsed.query)
    if "file" in qs:
        return unquote(qs["file"][0])
    path_name = Path(parsed.path).name
    return unquote(path_name) if path_name else "attachment"


def _build_local_index(folder: Path) -> dict[str, Path]:
    index: dict[str, Path] = {}
    if not folder.exists():
        print(f"Warning: local folder not found: {folder}")
        return index
    for path in folder.rglob("*"):
        if not path.is_file():
            continue
        full_key = _normalize_name(path.name)
        stem_key = _normalize_name(path.stem)
        if full_key and full_key not in index:
            index[full_key] = path
        if stem_key and stem_key not in index:
            index[stem_key] = path
    return index


def _find_local_attachment(url: str, index: dict[str, Path]) -> Path | None:
    if not url or not index:
        return None
    filename = _extract_filename_from_url(url)
    full_key = _normalize_name(filename)
    stem_key = _normalize_name(Path(filename).stem)
    if full_key in index:
        return index[full_key]
    if stem_key in index:
        return index[stem_key]
    return None


def _make_download_url(url: str) -> str:
    if not url:
        return url
    parsed = urlparse(url)
    qs = parse_qs(parsed.query)
    if "Doc.aspx" in parsed.path and "download" not in qs:
        qs["download"] = ["1"]
    if "action" in qs:
        qs["action"] = ["download"]
    query = "&".join(f"{k}={v[0]}" for k, v in qs.items())
    return parsed._replace(query=query).geturl()


def _download_attachment(
    url: str,
    dest_dir: Path,
    base_name: str,
    session: requests.Session | None = None,
) -> DownloadResult:
    if not url:
        return DownloadResult(False, None, "No URL provided", url=url)
    dest_dir.mkdir(parents=True, exist_ok=True)
    download_url = _make_download_url(url)
    client = session if session is not None else requests
    try:
        response = client.get(
            download_url,
            stream=True,
            timeout=30,
            headers={"User-Agent": "Mozilla/5.0"},
        )
    except Exception as exc:
        return DownloadResult(False, None, f"Request failed: {exc}", url=url)

    if response.status_code != 200:
        return DownloadResult(
            False,
            None,
            f"HTTP {response.status_code}",
            url=url,
        )

    content_type = response.headers.get("content-type", "").lower()
    filename = _extract_filename_from_url(url)
    suffix = Path(filename).suffix or ""
    if "application/pdf" in content_type:
        suffix = ".pdf"
    if not suffix:
        suffix = ".bin"
    safe_name = _sanitize_filename(base_name) + suffix
    out_path = dest_dir / safe_name

    # Some SharePoint responses return HTML when auth is required.
    if "text/html" in content_type and suffix not in {".html", ".htm"}:
        return DownloadResult(False, None, "Received HTML (auth required?)", url=url)

    try:
        with open(out_path, "wb") as handle:
            for chunk in response.iter_content(chunk_size=1024 * 64):
                if chunk:
                    handle.write(chunk)
    except Exception as exc:
        return DownloadResult(False, None, f"Write failed: {exc}", url=url)

    is_pdf = out_path.suffix.lower() == ".pdf" or "application/pdf" in content_type
    return DownloadResult(True, out_path, "Downloaded", url=url, is_pdf=is_pdf)


def _build_session(cookies_path: Path | None) -> requests.Session:
    session = requests.Session()
    if cookies_path:
        if cookies_path.exists():
            jar = cookiejar.MozillaCookieJar()
            jar.load(str(cookies_path), ignore_discard=True, ignore_expires=True)
            session.cookies.update(jar)
        else:
            print(f"Warning: cookies file not found: {cookies_path}")
    return session


def _find_soffice() -> Path | None:
    candidate = shutil.which("soffice")
    if candidate:
        return Path(candidate)
    for path in (
        Path("C:/Program Files/LibreOffice/program/soffice.exe"),
        Path("C:/Program Files (x86)/LibreOffice/program/soffice.exe"),
    ):
        if path.exists():
            return path
    return None


def _convert_with_soffice(input_path: Path, output_dir: Path) -> ConversionResult:
    soffice = _find_soffice()
    if not soffice:
        return ConversionResult(False, None, "LibreOffice (soffice) not found")
    output_dir.mkdir(parents=True, exist_ok=True)
    try:
        subprocess.run(
            [
                str(soffice),
                "--headless",
                "--convert-to",
                "pdf",
                "--outdir",
                str(output_dir),
                str(input_path),
            ],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
    except Exception as exc:
        return ConversionResult(False, None, f"LibreOffice conversion failed: {exc}")
    out_path = output_dir / f"{input_path.stem}.pdf"
    if out_path.exists():
        return ConversionResult(True, out_path, "Converted with LibreOffice")
    return ConversionResult(False, None, "LibreOffice conversion produced no PDF")


def _convert_with_docx2pdf(input_path: Path, output_dir: Path) -> ConversionResult:
    try:
        import docx2pdf  # type: ignore
    except Exception:
        return ConversionResult(False, None, "docx2pdf not installed")
    output_dir.mkdir(parents=True, exist_ok=True)
    try:
        docx2pdf.convert(str(input_path), str(output_dir))
    except Exception as exc:
        return ConversionResult(False, None, f"docx2pdf failed: {exc}")
    out_path = output_dir / f"{input_path.stem}.pdf"
    if out_path.exists():
        return ConversionResult(True, out_path, "Converted with docx2pdf")
    return ConversionResult(False, None, "docx2pdf produced no PDF")


def _convert_excel_with_win32(input_path: Path, output_dir: Path) -> ConversionResult:
    try:
        import win32com.client  # type: ignore
    except Exception:
        return ConversionResult(False, None, "win32com not installed")

    output_dir.mkdir(parents=True, exist_ok=True)
    sheet_pdfs: list[Path] = []
    excel = None
    workbook = None
    try:
        excel = win32com.client.DispatchEx("Excel.Application")
        excel.Visible = False
        excel.DisplayAlerts = False
        workbook = excel.Workbooks.Open(str(input_path), ReadOnly=True)
        for sheet in workbook.Worksheets:
            safe_sheet = _sanitize_filename(sheet.Name, "Sheet")
            out_path = output_dir / f"{input_path.stem}_{safe_sheet}.pdf"
            try:
                sheet.PageSetup.CenterHeader = sheet.Name
                sheet.PageSetup.Orientation = 2  # xlLandscape
                sheet.PageSetup.Zoom = False
                sheet.PageSetup.FitToPagesWide = 1
                sheet.PageSetup.FitToPagesTall = 1
            except Exception:
                pass
            try:
                sheet.ExportAsFixedFormat(0, str(out_path))
            except Exception as exc:
                return ConversionResult(False, None, f"Excel export failed: {exc}")
            if out_path.exists():
                sheet_pdfs.append(out_path)
    except Exception as exc:
        return ConversionResult(False, None, f"Excel automation failed: {exc}")
    finally:
        if workbook is not None:
            workbook.Close(False)
        if excel is not None:
            excel.Quit()

    if not sheet_pdfs:
        return ConversionResult(False, None, "Excel export produced no PDFs")

    merged_path = output_dir / f"{input_path.stem}.pdf"
    if HAVE_PYPDF:
        _merge_pdfs(sheet_pdfs, merged_path)
        for path in sheet_pdfs:
            try:
                path.unlink()
            except Exception:
                pass
        return ConversionResult(True, merged_path, "Converted with Excel")

    # If pypdf is unavailable, return first sheet as fallback.
    return ConversionResult(True, sheet_pdfs[0], "Converted with Excel (single sheet)")


def _convert_powerpoint_with_win32(input_path: Path, output_dir: Path) -> ConversionResult:
    try:
        import win32com.client  # type: ignore
        import pywintypes  # type: ignore
        import pythoncom  # type: ignore
    except Exception:
        return ConversionResult(False, None, "win32com not installed")

    output_dir.mkdir(parents=True, exist_ok=True)
    out_path = output_dir / f"{input_path.stem}.pdf"
    def _is_call_rejected(exc: Exception) -> bool:
        if isinstance(exc, pywintypes.com_error):
            return exc.hresult == -2147418111
        return "Call was rejected by callee" in str(exc)

    def _retry(fn, retries: int = 6, delay: float = 1.0):
        for attempt in range(retries):
            try:
                return fn()
            except Exception as exc:
                if _is_call_rejected(exc) and attempt < retries - 1:
                    time.sleep(delay)
                    continue
                raise

    powerpoint = None
    presentation = None
    try:
        pythoncom.CoInitialize()
        powerpoint = win32com.client.DispatchEx("PowerPoint.Application")
        powerpoint.Visible = False
        presentation = _retry(lambda: powerpoint.Presentations.Open(str(input_path), WithWindow=False))
        # 32 = ppSaveAsPDF
        _retry(lambda: presentation.SaveAs(str(out_path), 32))
    except Exception as exc:
        return ConversionResult(False, None, f"PowerPoint export failed: {exc}")
    finally:
        if presentation is not None:
            presentation.Close()
        if powerpoint is not None:
            powerpoint.Quit()
        try:
            pythoncom.CoUninitialize()
        except Exception:
            pass

    if out_path.exists():
        return ConversionResult(True, out_path, "Converted with PowerPoint")
    return ConversionResult(False, None, "PowerPoint export produced no PDF")


def _read_tabular(input_path: Path) -> pd.DataFrame:
    suffix = input_path.suffix.lower()
    if suffix in {".csv", ".tsv"}:
        try:
            return pd.read_csv(input_path)
        except Exception:
            return pd.read_csv(input_path, encoding="latin-1")
    return pd.read_excel(input_path)


def _convert_tabular_to_pdf(
    input_path: Path,
    output_dir: Path,
    base_font: str,
    bold_font: str,
) -> ConversionResult:
    try:
        df = _read_tabular(input_path)
    except Exception as exc:
        return ConversionResult(False, None, f"Failed to read tabular file: {exc}")

    output_dir.mkdir(parents=True, exist_ok=True)
    out_path = output_dir / f"{input_path.stem}.pdf"

    # Clean data for display
    df = df.fillna("")
    for col in df.columns:
        df[col] = df[col].astype(str).str.replace(r"\s+", " ", regex=True).str.strip()

    # Determine page orientation
    portrait_size = letter
    landscape_size = landscape(letter)
    max_cols = len(df.columns)

    def _measure_col_widths(font_name: str, font_size: float, max_sample_rows: int = 50):
        sample = df.head(max_sample_rows)
        widths = []
        for col in df.columns:
            header = str(col)
            max_text = header
            if not sample.empty:
                max_text = max([header] + sample[col].tolist(), key=len)
            width = stringWidth(str(max_text), font_name, font_size) + 8
            widths.append(width)
        return widths

    def _fit_layout(page_size):
        page_width, page_height = page_size
        left_margin = right_margin = 0.5 * inch
        top_margin = bottom_margin = 0.5 * inch
        title_space = 0.45 * inch
        avail_width = page_width - left_margin - right_margin
        avail_height = page_height - top_margin - bottom_margin - title_space
        font_size = 9.0
        min_font = 6.0

        while font_size >= min_font:
            row_height = font_size + 4
            max_rows = int(avail_height // row_height) - 1  # header row
            if max_rows < 1:
                font_size -= 0.5
                continue
            total_rows = len(df)
            fits_rows = total_rows <= max_rows
            widths = _measure_col_widths(base_font, font_size)
            total_width = sum(widths)
            fits_cols = total_width <= avail_width
            if fits_rows and fits_cols:
                return font_size, max_rows, widths, avail_width
            font_size -= 0.5

        # Force fit by scaling columns, truncate rows
        font_size = min_font
        row_height = font_size + 4
        max_rows = max(1, int(avail_height // row_height) - 1)
        widths = _measure_col_widths(base_font, font_size)
        total_width = sum(widths)
        if total_width > 0:
            scale = min(1.0, avail_width / total_width)
            widths = [w * scale for w in widths]
        return font_size, max_rows, widths, avail_width

    page_size = landscape_size if max_cols > 8 else portrait_size
    font_size, max_rows, widths, avail_width = _fit_layout(page_size)

    # If still too wide in portrait, switch to landscape and refit
    if sum(widths) > avail_width and page_size == portrait_size:
        page_size = landscape_size
        font_size, max_rows, widths, avail_width = _fit_layout(page_size)

    # Truncate if needed to fit one page
    truncated = False
    if len(df) > max_rows:
        df = df.head(max_rows)
        truncated = True

    # Build table data
    header_row = [str(col) for col in df.columns]
    data_rows = df.values.tolist()
    if truncated:
        data_rows.append([f"... truncated to fit page (showing first {max_rows} rows)" + (" " * 2)] + [""] * (len(header_row) - 1))

    data = [header_row] + data_rows

    doc = SimpleDocTemplate(
        str(out_path),
        pagesize=page_size,
        leftMargin=0.5 * inch,
        rightMargin=0.5 * inch,
        topMargin=0.5 * inch,
        bottomMargin=0.5 * inch,
        title=input_path.name,
    )

    title_style = ParagraphStyle(
        name="TabularTitle",
        fontName=bold_font,
        fontSize=12,
        alignment=TA_CENTER,
        spaceAfter=6,
    )
    body_style = ParagraphStyle(
        name="TabularBody",
        fontName=base_font,
        fontSize=font_size,
        alignment=TA_LEFT,
    )
    header_style = ParagraphStyle(
        name="TabularHeader",
        fontName=bold_font,
        fontSize=font_size,
        alignment=TA_LEFT,
    )

    table = Table(data, colWidths=widths, hAlign="LEFT")
    table.setStyle(TableStyle([
        ("BACKGROUND", (0, 0), (-1, 0), colors.HexColor("#E9ECEF")),
        ("TEXTCOLOR", (0, 0), (-1, 0), colors.black),
        ("FONTNAME", (0, 0), (-1, 0), bold_font),
        ("FONTSIZE", (0, 0), (-1, -1), font_size),
        ("ALIGN", (0, 0), (-1, -1), "LEFT"),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
        ("GRID", (0, 0), (-1, -1), 0.3, colors.HexColor("#CED4DA")),
        ("LEFTPADDING", (0, 0), (-1, -1), 3),
        ("RIGHTPADDING", (0, 0), (-1, -1), 3),
        ("TOPPADDING", (0, 0), (-1, -1), 2),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 2),
    ]))

    story = [Paragraph(xml_escape(input_path.name), title_style), table]
    doc.build(story)
    return ConversionResult(True, out_path, "Converted tabular file")


def _convert_to_pdf(
    input_path: Path,
    output_dir: Path,
    base_font: str,
    bold_font: str,
) -> ConversionResult:
    suffix = input_path.suffix.lower()
    if suffix == ".pdf":
        return ConversionResult(True, input_path, "Already PDF")

    if suffix in {".xlsx", ".xls", ".xlsm"}:
        result = _convert_excel_with_win32(input_path, output_dir)
        if result.ok:
            return result

    if suffix in {".ppt", ".pptx", ".pptm"}:
        result = _convert_powerpoint_with_win32(input_path, output_dir)
        if result.ok:
            return result

    if suffix in {".csv", ".tsv", ".xlsx", ".xls", ".xlsm"}:
        result = _convert_tabular_to_pdf(input_path, output_dir, base_font, bold_font)
        if result.ok:
            return result

    # Try LibreOffice first for broad format coverage
    result = _convert_with_soffice(input_path, output_dir)
    if result.ok:
        return result

    # Fallback for Word docs when LibreOffice is not available
    if suffix in {".doc", ".docx"}:
        return _convert_with_docx2pdf(input_path, output_dir)

    return result


def _build_styles():
    styles = getSampleStyleSheet()
    base_font = "Helvetica"
    bold_font = "Helvetica-Bold"
    try:
        fonts_dir = Path("C:/Windows/Fonts")
        regular_font = fonts_dir / "times.ttf"
        bold_ttf = fonts_dir / "timesbd.ttf"
        if regular_font.exists():
            pdfmetrics.registerFont(TTFont("TimesNewRoman", str(regular_font)))
            base_font = "TimesNewRoman"
        if bold_ttf.exists():
            pdfmetrics.registerFont(TTFont("TimesNewRomanBold", str(bold_ttf)))
            bold_font = "TimesNewRomanBold"
    except Exception:
        base_font = "Helvetica"
        bold_font = "Helvetica-Bold"

    for style_name in styles.byName:
        styles[style_name].fontName = base_font
        styles[style_name].textColor = colors.black
    styles["Normal"].fontSize = 9
    styles["Normal"].leading = 12

    title_style = ParagraphStyle(
        name="Title",
        parent=styles["Title"],
        fontName=bold_font,
        fontSize=16,
        alignment=TA_CENTER,
        textColor=colors.HexColor("#2B3A42"),
        spaceAfter=12,
    )
    label_style = ParagraphStyle(
        name="Label",
        parent=styles["Normal"],
        fontName=bold_font,
        fontSize=9,
        alignment=TA_LEFT,
        textColor=colors.HexColor("#1F2A30"),
    )
    value_style = ParagraphStyle(
        name="Value",
        parent=styles["Normal"],
        fontName=base_font,
        fontSize=9,
        alignment=TA_LEFT,
        textColor=colors.black,
    )
    subtitle_style = ParagraphStyle(
        name="Subtitle",
        parent=styles["Normal"],
        fontName=base_font,
        fontSize=10,
        alignment=TA_CENTER,
        textColor=colors.HexColor("#4A4A4A"),
        spaceAfter=8,
    )
    cover_title = ParagraphStyle(
        name="CoverTitle",
        parent=styles["Title"],
        fontName=bold_font,
        fontSize=22,
        alignment=TA_CENTER,
        textColor=colors.HexColor("#2B3A42"),
        spaceAfter=14,
    )
    cover_subtitle = ParagraphStyle(
        name="CoverSubtitle",
        parent=styles["Normal"],
        fontName=base_font,
        fontSize=14,
        alignment=TA_CENTER,
        textColor=colors.HexColor("#2B3A42"),
        spaceAfter=10,
    )
    cover_meta = ParagraphStyle(
        name="CoverMeta",
        parent=styles["Normal"],
        fontName=base_font,
        fontSize=12,
        alignment=TA_CENTER,
        textColor=colors.HexColor("#1F2A30"),
        spaceAfter=6,
    )
    toc_title = ParagraphStyle(
        name="TOCTitle",
        parent=styles["Title"],
        fontName=bold_font,
        fontSize=18,
        alignment=TA_CENTER,
        textColor=colors.HexColor("#2B3A42"),
        spaceAfter=18,
    )
    toc_project = ParagraphStyle(
        name="TOCProject",
        parent=styles["Normal"],
        fontName=bold_font,
        fontSize=10,
        alignment=TA_LEFT,
        textColor=colors.HexColor("#1F2A30"),
        spaceBefore=6,
        spaceAfter=1,
        leftIndent=0,
    )
    toc_sub = ParagraphStyle(
        name="TOCSub",
        parent=styles["Normal"],
        fontName=base_font,
        fontSize=9,
        alignment=TA_LEFT,
        textColor=colors.HexColor("#4A4A4A"),
        spaceBefore=1,
        spaceAfter=1,
        leftIndent=18,
    )
    return {
        "base_font": base_font,
        "bold_font": bold_font,
        "title": title_style,
        "label": label_style,
        "value": value_style,
        "subtitle": subtitle_style,
        "cover_title": cover_title,
        "cover_subtitle": cover_subtitle,
        "cover_meta": cover_meta,
        "toc_title": toc_title,
        "toc_project": toc_project,
        "toc_sub": toc_sub,
    }


def _format_value(value: str) -> str:
    text = _safe_str(value)
    if not text:
        return "Not provided"
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    text = xml_escape(text).replace("\n", "<br/>")
    return text


def _build_title_page_pdf(output_path: Path, year_value: str, total_count: int, styles: dict) -> None:
    doc = SimpleDocTemplate(
        str(output_path),
        pagesize=letter,
        leftMargin=0.65 * inch,
        rightMargin=0.65 * inch,
        topMargin=0.65 * inch,
        bottomMargin=0.65 * inch,
        title="AEA Research Project Report",
    )
    year_text = year_value or "2026"
    story = [
        Spacer(1, 2.2 * inch),
        Paragraph(f"{xml_escape(year_text)} Project Request", styles["cover_title"]),
        Paragraph("Agriculture Engineering and Agronomy Farm", styles["cover_subtitle"]),
        Paragraph(xml_escape(year_text), styles["cover_subtitle"]),
        Spacer(1, 0.3 * inch),
        Paragraph(f"Total Project Request: {total_count}", styles["cover_meta"]),
        Spacer(1, 0.15 * inch),
        Paragraph(f"Date Printed: {datetime.now().strftime('%B %d, %Y')}", styles["cover_meta"]),
    ]
    doc.build(story)


@dataclass
class _TOCEntry:
    project_title: str
    project_page: int
    protocol_page: int | None = None
    plotmap_page: int | None = None


def _count_pdf_pages(path: Path) -> int:
    reader = PdfReader(str(path))
    return len(reader.pages)


def _dot_leader(label: str, page_num: int, font_name: str, font_size: float, avail_width: float) -> str:
    """Return a string like: Label .............. 5"""
    page_str = str(page_num)
    label_width = stringWidth(label, font_name, font_size)
    page_width = stringWidth(page_str, font_name, font_size)
    space_width = stringWidth(" ", font_name, font_size)
    dot_width = stringWidth(".", font_name, font_size)
    # Reserve space for: label + space + dots + space + page number + safety margin
    space_for_dots = avail_width - label_width - page_width - (space_width * 2) - 24
    num_dots = max(3, int(space_for_dots / dot_width))
    dots = " " + "." * num_dots + " "
    return f'{xml_escape(label)}{dots}{page_str}'


def _build_toc_pdf(
    output_path: Path,
    entries: list[_TOCEntry],
    styles: dict,
) -> None:
    """Build a TOC PDF with plain text. Returns list of (page_idx_within_toc, y, height, target_page) for link overlay."""
    doc = SimpleDocTemplate(
        str(output_path),
        pagesize=letter,
        leftMargin=0.65 * inch,
        rightMargin=0.65 * inch,
        topMargin=0.65 * inch,
        bottomMargin=0.65 * inch,
        title="Table of Contents",
    )
    avail_width = doc.width
    story: list = [
        Paragraph("Table of Contents", styles["toc_title"]),
    ]

    base_font = styles["base_font"]
    bold_font = styles["bold_font"]

    for entry in entries:
        project_line = _dot_leader(
            entry.project_title,
            entry.project_page,
            bold_font,
            10,
            avail_width,
        )
        story.append(Paragraph(project_line, styles["toc_project"]))

        if entry.protocol_page is not None:
            proto_line = _dot_leader(
                "Protocol",
                entry.protocol_page,
                base_font,
                9,
                avail_width - 18,
            )
            story.append(Paragraph(proto_line, styles["toc_sub"]))

        if entry.plotmap_page is not None:
            map_line = _dot_leader(
                "Plot Map",
                entry.plotmap_page,
                base_font,
                9,
                avail_width - 18,
            )
            story.append(Paragraph(map_line, styles["toc_sub"]))

    doc.build(story)


def _add_toc_links(
    input_path: Path,
    output_path: Path,
    entries: list[_TOCEntry],
    title_pages: int,
    toc_page_count: int,
) -> None:
    """Overlay transparent clickable link annotations on the TOC pages of the merged PDF."""
    from pypdf.generic import (
        ArrayObject,
        DictionaryObject,
        FloatObject,
        NameObject,
        NumberObject,
    )

    reader = PdfReader(str(input_path))
    writer = PdfWriter()
    for page in reader.pages:
        writer.add_page(page)

    # Build the flat list of TOC lines and their target pages
    toc_lines: list[tuple[str, int]] = []
    for entry in entries:
        toc_lines.append((entry.project_title, entry.project_page))
        if entry.protocol_page is not None:
            toc_lines.append(("Protocol", entry.protocol_page))
        if entry.plotmap_page is not None:
            toc_lines.append(("Plot Map", entry.plotmap_page))

    if not toc_lines:
        # Nothing to link — just copy through
        with open(output_path, "wb") as handle:
            writer.write(handle)
        return

    # Estimate line positions on each TOC page.
    # Page layout: 0.65" margins, title ~30pt + 18pt spaceAfter = ~48pt from top.
    # Each project line: ~10pt font + 6pt spaceBefore + 1pt spaceAfter ≈ 17pt
    # Each sub line: ~9pt font + 1pt spaceBefore + 1pt spaceAfter ≈ 11pt
    page_width = float(letter[0])
    page_height = float(letter[1])
    left_margin = 0.65 * 72  # points
    right_margin = 0.65 * 72
    top_margin = 0.65 * 72
    bottom_margin = 0.65 * 72

    # Starting y from top of content area, after title paragraph
    title_height = 48  # approximate: 18pt font + leading + spaceAfter
    content_top = page_height - top_margin - title_height
    usable_height = content_top - bottom_margin

    # Compute height for each line
    line_heights: list[float] = []
    for label, _target in toc_lines:
        if label not in ("Protocol", "Plot Map"):
            line_heights.append(19.0)  # project line: 10pt + spaceBefore(6) + spaceAfter(1) + leading
        else:
            line_heights.append(13.0)  # sub line: 9pt + spaceBefore(1) + spaceAfter(1) + leading

    # Distribute lines across TOC pages
    toc_start_page_idx = title_pages  # 0-based index of first TOC page in merged PDF
    current_y = content_top
    current_toc_page = 0

    for i, (label, target_page) in enumerate(toc_lines):
        h = line_heights[i]
        if current_y - h < bottom_margin and i > 0:
            # Overflow to next TOC page
            current_toc_page += 1
            current_y = page_height - top_margin  # no title on continuation pages

        rect_top = current_y
        rect_bottom = current_y - h
        current_y = rect_bottom

        # target_page is 1-based absolute page number in the final PDF
        target_page_idx = target_page - 1  # 0-based for pypdf

        # Create link annotation
        annot = DictionaryObject()
        annot[NameObject("/Type")] = NameObject("/Annot")
        annot[NameObject("/Subtype")] = NameObject("/Link")
        annot[NameObject("/Rect")] = ArrayObject([
            FloatObject(left_margin),
            FloatObject(rect_bottom),
            FloatObject(page_width - right_margin),
            FloatObject(rect_top),
        ])
        annot[NameObject("/Border")] = ArrayObject([
            NumberObject(0), NumberObject(0), NumberObject(0),
        ])
        # GoTo action to target page
        dest = ArrayObject([
            writer.pages[target_page_idx].indirect_reference,
            NameObject("/Fit"),
        ])
        action = DictionaryObject()
        action[NameObject("/Type")] = NameObject("/Action")
        action[NameObject("/S")] = NameObject("/GoTo")
        action[NameObject("/D")] = dest
        annot[NameObject("/A")] = action

        # Add annotation to the correct TOC page
        page_idx = toc_start_page_idx + current_toc_page
        if page_idx < len(writer.pages):
            page_obj = writer.pages[page_idx]
            if "/Annots" not in page_obj:
                page_obj[NameObject("/Annots")] = ArrayObject()
            page_obj[NameObject("/Annots")].append(
                writer._add_object(annot)
            )

    with open(output_path, "wb") as handle:
        writer.write(handle)


def _build_row_pdf(row: pd.Series, output_path: Path, year_value: str, styles: dict) -> None:
    doc = SimpleDocTemplate(
        str(output_path),
        pagesize=letter,
        leftMargin=0.65 * inch,
        rightMargin=0.65 * inch,
        topMargin=0.65 * inch,
        bottomMargin=0.65 * inch,
        title="AEA Research Project Report",
    )
    project_title = _safe_str(row.get("Project Title"))
    title_text = project_title or "Project"
    if year_value:
        title_text = f"{title_text} ({year_value})"
    story = [Paragraph(xml_escape(title_text), styles["title"])]

    data = []
    for col_name, label in LABEL_MAP:
        value = _format_value(row.get(col_name))
        data.append([
            Paragraph(xml_escape(label), styles["label"]),
            Paragraph(value, styles["value"]),
        ])

    table = Table(
        data,
        colWidths=[doc.width * 0.33, doc.width * 0.67],
        hAlign="LEFT",
    )
    table.setStyle(TableStyle([
        ("BACKGROUND", (0, 0), (0, -1), colors.HexColor("#F1F3F5")),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
        ("GRID", (0, 0), (-1, -1), 0.3, colors.HexColor("#CED4DA")),
        ("LEFTPADDING", (0, 0), (-1, -1), 6),
        ("RIGHTPADDING", (0, 0), (-1, -1), 6),
        ("TOPPADDING", (0, 0), (-1, -1), 4),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 4),
    ]))

    story.append(table)
    doc.build(story)


def _build_attachment_notice(
    output_path: Path,
    title: str,
    message: str,
    url: str | None,
    styles: dict,
) -> None:
    doc = SimpleDocTemplate(
        str(output_path),
        pagesize=letter,
        leftMargin=0.65 * inch,
        rightMargin=0.65 * inch,
        topMargin=0.65 * inch,
        bottomMargin=0.65 * inch,
        title="AEA Research Project Attachment",
    )
    story = [
        Paragraph(xml_escape(title), styles["title"]),
        Paragraph(xml_escape(message), styles["subtitle"]),
    ]
    if url:
        safe_url = xml_escape(url)
        story.append(Paragraph(f'<a href="{safe_url}">{safe_url}</a>', styles["value"]))
    doc.build(story)


def _merge_pdfs(pdf_paths: Iterable[Path], output_path: Path) -> None:
    writer = PdfWriter()
    for pdf_path in pdf_paths:
        reader = PdfReader(str(pdf_path))
        for page in reader.pages:
            writer.add_page(page)
    with open(output_path, "wb") as handle:
        writer.write(handle)


def _add_page_numbers(input_path: Path, output_path: Path, font_name: str, skip_pages: int = 0) -> None:
    reader = PdfReader(str(input_path))
    writer = PdfWriter()
    for idx, page in enumerate(reader.pages, start=1):
        if idx <= skip_pages:
            writer.add_page(page)
            continue
        packet = io.BytesIO()
        width = float(page.mediabox.width)
        height = float(page.mediabox.height)
        canvas = rl_canvas.Canvas(packet, pagesize=(width, height))
        try:
            canvas.setFont(font_name, 9)
        except Exception:
            canvas.setFont("Times-Roman", 9)
        canvas.drawCentredString(width / 2.0, 0.5 * inch, str(idx))
        canvas.save()
        packet.seek(0)
        overlay = PdfReader(packet).pages[0]
        page.merge_page(overlay)
        writer.add_page(page)
    with open(output_path, "wb") as handle:
        writer.write(handle)


def _validate_columns(df: pd.DataFrame) -> None:
    missing = [col for col in REQUIRED_COLS if col not in df.columns]
    if missing:
        raise ValueError(f"Missing required columns: {', '.join(missing)}")


def _filter_year(df: pd.DataFrame, year: str) -> pd.DataFrame:
    year_num = pd.to_numeric(df["ReportingYear"], errors="coerce")
    mask = year_num.eq(int(year))
    if mask.sum() == 0:
        mask = df["ReportingYear"].astype(str).str.strip() == str(year)
    return df.loc[mask].copy()


def _build_base_name(row: pd.Series, suffix_label: str) -> str:
    pi_last = _sanitize_filename(_safe_str(row.get("Faculty PI Last Name")), "PI")
    project = _sanitize_filename(_safe_str(row.get("Project Title")), "Project")
    field = _sanitize_filename(_pick_field_name(row), "Field")
    return f"{pi_last}_{project}_{field}_{suffix_label}"


def generate_report(
    input_path: Path,
    output_path: Path,
    year: str,
    attachments_dir: Path,
    temp_dir: Path,
    protocol_dir: Path | None = None,
    plotmap_dir: Path | None = None,
    cookies_path: Path | None = None,
    keep_temp: bool = False,
) -> None:
    df = pd.read_excel(input_path)
    _validate_columns(df)
    filtered = _filter_year(df, year)
    if filtered.empty:
        raise ValueError(f"No rows found for ReportingYear={year}")

    styles = _build_styles()

    filtered = filtered.copy()
    filtered["_pi_sort"] = filtered["Faculty PI Last Name"].apply(lambda v: _safe_str(v).lower())
    filtered.sort_values(by="_pi_sort", kind="mergesort", inplace=True)
    filtered.drop(columns=["_pi_sort"], inplace=True)

    title_page_path = temp_dir / "title_page.pdf"
    _build_title_page_pdf(title_page_path, year, len(filtered), styles)

    session = _build_session(cookies_path)
    protocol_index = _build_local_index(protocol_dir) if protocol_dir else {}
    plotmap_index = _build_local_index(plotmap_dir) if plotmap_dir else {}

    # Build the content PDFs and track where each piece lands.
    # Each element is (path, tag) where tag is one of:
    #   ("project", seq, title)  ("protocol", seq)  ("plotmap", seq)  (None,)
    content_items: list[tuple[Path, tuple]] = []

    for seq, (_, row) in enumerate(filtered.iterrows(), start=1):
        row_year = _safe_str(row.get("ReportingYear")) or year
        project_title = _safe_str(row.get("Project Title")) or "Project"

        row_pdf = temp_dir / f"row_{seq}.pdf"
        _build_row_pdf(row, row_pdf, row_year, styles)
        content_items.append((row_pdf, ("project", seq, project_title)))

        # Protocol attachment
        protocol_url = _safe_str(row.get(PROJECT_PROTOCOL_COL))
        if protocol_url:
            base_name = _build_base_name(row, "Protocols")
            local_path = _find_local_attachment(protocol_url, protocol_index)
            if local_path and local_path.exists():
                result = DownloadResult(True, local_path, "Using local file", url=protocol_url, is_pdf=local_path.suffix.lower() == ".pdf")
            else:
                result = _download_attachment(protocol_url, attachments_dir, base_name, session=session)
            if result.ok and result.path:
                if result.is_pdf and HAVE_PYPDF:
                    content_items.append((result.path, ("protocol", seq)))
                else:
                    conversion = _convert_to_pdf(result.path, attachments_dir, styles["base_font"], styles["bold_font"])
                    if conversion.ok and conversion.path and HAVE_PYPDF:
                        content_items.append((conversion.path, ("protocol", seq)))
                    else:
                        msg = conversion.message
                        if result.ok and result.path and not result.is_pdf:
                            msg = f"{conversion.message}. Downloaded: {result.path.name}"
                        notice_path = temp_dir / f"row_{seq}_protocol_notice.pdf"
                        _build_attachment_notice(
                            notice_path,
                            "Attachment: Project Protocol",
                            msg,
                            result.url,
                            styles,
                        )
                        content_items.append((notice_path, ("protocol", seq)))
            else:
                notice_path = temp_dir / f"row_{seq}_protocol_notice.pdf"
                _build_attachment_notice(
                    notice_path,
                    "Attachment: Project Protocol",
                    result.message,
                    result.url,
                    styles,
                )
                content_items.append((notice_path, ("protocol", seq)))

        # Plot map attachment
        plot_map_flag = _safe_str(row.get(PLOT_MAP_FLAG_COL))
        if _is_yes(plot_map_flag):
            plot_map_url = _safe_str(row.get(PLOT_MAP_COL))
            if plot_map_url:
                base_name = _build_base_name(row, "PlotMap")
                local_path = _find_local_attachment(plot_map_url, plotmap_index)
                if local_path and local_path.exists():
                    result = DownloadResult(True, local_path, "Using local file", url=plot_map_url, is_pdf=local_path.suffix.lower() == ".pdf")
                else:
                    result = _download_attachment(plot_map_url, attachments_dir, base_name, session=session)
                if result.ok and result.path:
                    if result.is_pdf and HAVE_PYPDF:
                        content_items.append((result.path, ("plotmap", seq)))
                    else:
                        conversion = _convert_to_pdf(result.path, attachments_dir, styles["base_font"], styles["bold_font"])
                        if conversion.ok and conversion.path and HAVE_PYPDF:
                            content_items.append((conversion.path, ("plotmap", seq)))
                        else:
                            msg = conversion.message
                            if result.ok and result.path and not result.is_pdf:
                                msg = f"{conversion.message}. Downloaded: {result.path.name}"
                            notice_path = temp_dir / f"row_{seq}_plotmap_notice.pdf"
                            _build_attachment_notice(
                                notice_path,
                                "Attachment: Plot Map",
                                msg,
                                result.url,
                                styles,
                            )
                            content_items.append((notice_path, ("plotmap", seq)))
                else:
                    notice_path = temp_dir / f"row_{seq}_plotmap_notice.pdf"
                    _build_attachment_notice(
                        notice_path,
                        "Attachment: Plot Map",
                        result.message,
                        result.url,
                        styles,
                    )
                    content_items.append((notice_path, ("plotmap", seq)))
            else:
                notice_path = temp_dir / f"row_{seq}_plotmap_notice.pdf"
                _build_attachment_notice(
                    notice_path,
                    "Attachment: Plot Map",
                    "Plot map marked as Yes, but no URL found.",
                    None,
                    styles,
                )
                content_items.append((notice_path, ("plotmap", seq)))

    if not HAVE_PYPDF:
        raise RuntimeError("pypdf is required to merge pages. Install with: python -m pip install pypdf")

    # ---- Compute page numbers for each content item ----
    title_pages = _count_pdf_pages(title_page_path)
    # We'll insert the TOC after the title page. To figure out TOC page count,
    # we first build the TOC with preliminary page numbers, count its pages,
    # then rebuild with corrected numbers.

    # Cumulative page offset within content (not counting title or TOC yet)
    content_page_counts: list[int] = []
    for path, _tag in content_items:
        content_page_counts.append(_count_pdf_pages(path))

    # Pass 1: compute page numbers assuming 0 TOC pages, then build TOC to measure it
    def _compute_toc_entries(toc_page_count: int) -> list[_TOCEntry]:
        base_offset = title_pages + toc_page_count  # pages before content starts
        cumulative = 0
        # Track per-seq data
        project_pages: dict[int, tuple[str, int]] = {}  # seq -> (title, page)
        protocol_pages: dict[int, int] = {}
        plotmap_pages: dict[int, int] = {}

        for idx, (path, tag) in enumerate(content_items):
            page_num = base_offset + cumulative + 1  # 1-based
            if tag[0] == "project":
                _seq, title = tag[1], tag[2]
                project_pages[_seq] = (title, page_num)
            elif tag[0] == "protocol":
                protocol_pages[tag[1]] = page_num
            elif tag[0] == "plotmap":
                plotmap_pages[tag[1]] = page_num
            cumulative += content_page_counts[idx]

        entries = []
        for seq in sorted(project_pages.keys()):
            title, page = project_pages[seq]
            entries.append(_TOCEntry(
                project_title=title,
                project_page=page,
                protocol_page=protocol_pages.get(seq),
                plotmap_page=plotmap_pages.get(seq),
            ))
        return entries

    # Pass 1 — build TOC with estimated 1 page, measure actual page count
    toc_path = temp_dir / "toc.pdf"
    entries_pass1 = _compute_toc_entries(toc_page_count=1)
    _build_toc_pdf(toc_path, entries_pass1, styles)
    toc_pages = _count_pdf_pages(toc_path)

    # Pass 2 — rebuild with correct toc_pages if it changed
    final_entries = entries_pass1
    if toc_pages != 1:
        final_entries = _compute_toc_entries(toc_page_count=toc_pages)
        _build_toc_pdf(toc_path, final_entries, styles)
        toc_pages_check = _count_pdf_pages(toc_path)
        # If page count changed again (unlikely), do one more pass
        if toc_pages_check != toc_pages:
            toc_pages = toc_pages_check
            final_entries = _compute_toc_entries(toc_page_count=toc_pages)
            _build_toc_pdf(toc_path, final_entries, styles)

    # ---- Assemble final sequence: title + TOC + content ----
    pdf_sequence = [title_page_path, toc_path] + [path for path, _tag in content_items]

    merged_path = output_path.with_name(f"{output_path.stem}_merged.pdf")
    _merge_pdfs(pdf_sequence, merged_path)

    # Add clickable link annotations on TOC pages
    linked_path = output_path.with_name(f"{output_path.stem}_linked.pdf")
    _add_toc_links(merged_path, linked_path, final_entries, title_pages, toc_pages)

    # Copy linked PDF as final output
    shutil.copy2(linked_path, output_path)

    if not keep_temp:
        for path in temp_dir.glob("*.pdf"):
            try:
                path.unlink()
            except Exception:
                pass
        for tmp in (merged_path, linked_path):
            if tmp.exists():
                try:
                    tmp.unlink()
                except Exception:
                    pass


def _parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate AEA Research Project PDF report.")
    parser.add_argument(
        "--input",
        required=True,
        help="Path to the Excel file.",
    )
    parser.add_argument(
        "--output",
        required=False,
        help="Output PDF path.",
    )
    parser.add_argument(
        "--year",
        default="2026",
        help="ReportingYear filter (default: 2026).",
    )
    parser.add_argument(
        "--attachments-dir",
        required=False,
        help="Directory to store downloaded attachments.",
    )
    parser.add_argument(
        "--temp-dir",
        required=False,
        help="Directory for temporary PDF pages.",
    )
    parser.add_argument(
        "--protocol-dir",
        required=False,
        help="Local folder containing Project Protocol files.",
    )
    parser.add_argument(
        "--plotmap-dir",
        required=False,
        help="Local folder containing Plot Map files.",
    )
    parser.add_argument(
        "--cookies",
        required=False,
        help="Path to a Netscape cookies.txt file for SharePoint auth.",
    )
    parser.add_argument(
        "--keep-temp",
        action="store_true",
        help="Keep temp PDFs for inspection.",
    )
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = _parse_args(argv)
    input_path = Path(args.input)
    if not input_path.exists():
        print(f"Input not found: {input_path}")
        return 2

    output_path = Path(args.output) if args.output else input_path.with_name("AEA_Research_Project_Report_2026.pdf")
    attachments_dir = Path(args.attachments_dir) if args.attachments_dir else output_path.with_name("AEA_Report_Attachments")
    temp_dir = Path(args.temp_dir) if args.temp_dir else output_path.with_name("AEA_Report_Temp")
    temp_dir.mkdir(parents=True, exist_ok=True)
    cookies_path = Path(args.cookies) if args.cookies else None
    protocol_dir = Path(args.protocol_dir) if args.protocol_dir else None
    plotmap_dir = Path(args.plotmap_dir) if args.plotmap_dir else None

    try:
        generate_report(
            input_path=input_path,
            output_path=output_path,
            year=str(args.year),
            attachments_dir=attachments_dir,
            temp_dir=temp_dir,
            protocol_dir=protocol_dir,
            plotmap_dir=plotmap_dir,
            cookies_path=cookies_path,
            keep_temp=args.keep_temp,
        )
    except Exception as exc:
        print(f"Report generation failed: {exc}")
        return 1

    print(f"Report written to: {output_path}")
    print(f"Attachments directory: {attachments_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))