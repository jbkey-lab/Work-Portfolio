import argparse
import csv
import sys
from pathlib import Path
from xml.etree import ElementTree as ET
from zipfile import ZIP_DEFLATED, ZipFile


DEFAULT_OUTPUT = Path.home() / "Documents" / "AEAInvoice" / "AEA_Project_Leaders.xlsx"
MAIN_NS = {"main": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}
REL_NS = {"rel": "http://schemas.openxmlformats.org/package/2006/relationships"}
DOC_REL_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"


def _safe_str(value) -> str:
    if value is None:
        return ""
    return str(value).strip()


def _parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Export project leader names and project titles to Excel."
    )
    parser.add_argument("--input", required=True, help="Path to the source file.")
    parser.add_argument(
        "--output",
        default=str(DEFAULT_OUTPUT),
        help=f"Output Excel path (default: {DEFAULT_OUTPUT}).",
    )
    parser.add_argument("--year", help="Optional ReportingYear filter.")
    return parser.parse_args(argv)


def _read_csv_rows(input_path: Path) -> list[dict[str, str]]:
    with input_path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        return [{_safe_str(k): _safe_str(v) for k, v in row.items()} for row in reader]


def _column_letters_to_index(ref: str) -> int:
    letters = "".join(ch for ch in ref if ch.isalpha()).upper()
    value = 0
    for ch in letters:
        value = (value * 26) + (ord(ch) - ord("A") + 1)
    return value - 1


def _load_shared_strings(archive: ZipFile) -> list[str]:
    if "xl/sharedStrings.xml" not in archive.namelist():
        return []
    root = ET.fromstring(archive.read("xl/sharedStrings.xml"))
    values: list[str] = []
    for item in root.findall("main:si", MAIN_NS):
        parts = [node.text or "" for node in item.findall(".//main:t", MAIN_NS)]
        values.append("".join(parts))
    return values


def _first_sheet_path(archive: ZipFile) -> str:
    workbook_root = ET.fromstring(archive.read("xl/workbook.xml"))
    sheets = workbook_root.find("main:sheets", MAIN_NS)
    if sheets is None or not list(sheets):
        raise ValueError("No worksheets found in workbook")

    first_sheet = list(sheets)[0]
    rel_id = first_sheet.attrib.get(f"{{{DOC_REL_NS}}}id")
    if not rel_id:
        raise ValueError("Worksheet relationship id not found")

    rels_root = ET.fromstring(archive.read("xl/_rels/workbook.xml.rels"))
    for rel in rels_root.findall("rel:Relationship", REL_NS):
        if rel.attrib.get("Id") == rel_id:
            target = rel.attrib.get("Target", "")
            target = target.lstrip("/")
            if not target.startswith("xl/"):
                target = f"xl/{target}"
            return target
    raise ValueError("Worksheet path not found")


def _cell_value(cell: ET.Element, shared_strings: list[str]) -> str:
    cell_type = cell.attrib.get("t")
    if cell_type == "inlineStr":
        texts = [node.text or "" for node in cell.findall(".//main:t", MAIN_NS)]
        return "".join(texts).strip()

    value_node = cell.find("main:v", MAIN_NS)
    if value_node is None or value_node.text is None:
        return ""

    raw_value = value_node.text
    if cell_type == "s":
        idx = int(raw_value)
        return shared_strings[idx].strip() if 0 <= idx < len(shared_strings) else ""
    return raw_value.strip()


def _read_xlsx_rows(input_path: Path) -> list[dict[str, str]]:
    with ZipFile(input_path) as archive:
        shared_strings = _load_shared_strings(archive)
        sheet_path = _first_sheet_path(archive)
        sheet_root = ET.fromstring(archive.read(sheet_path))

    rows: list[list[str]] = []
    for row in sheet_root.findall(".//main:sheetData/main:row", MAIN_NS):
        values_by_index: dict[int, str] = {}
        for cell in row.findall("main:c", MAIN_NS):
            ref = cell.attrib.get("r", "")
            if not ref:
                continue
            values_by_index[_column_letters_to_index(ref)] = _cell_value(cell, shared_strings)
        if not values_by_index:
            rows.append([])
            continue
        row_values = [""] * (max(values_by_index) + 1)
        for idx, value in values_by_index.items():
            row_values[idx] = value
        rows.append(row_values)

    if not rows:
        return []

    headers = [_safe_str(value) for value in rows[0]]
    records: list[dict[str, str]] = []
    for row_values in rows[1:]:
        record: dict[str, str] = {}
        for idx, header in enumerate(headers):
            if not header:
                continue
            record[header] = _safe_str(row_values[idx] if idx < len(row_values) else "")
        records.append(record)
    return records


def _load_rows(input_path: Path) -> list[dict[str, str]]:
    suffix = input_path.suffix.lower()
    if suffix == ".csv":
        return _read_csv_rows(input_path)
    if suffix == ".xlsx":
        return _read_xlsx_rows(input_path)
    raise ValueError("Only .xlsx and .csv inputs are supported")


def _filter_year(rows: list[dict[str, str]], year: str | None) -> list[dict[str, str]]:
    if not year:
        return list(rows)
    filtered: list[dict[str, str]] = []
    for row in rows:
        if _safe_str(row.get("ReportingYear")) == str(year):
            filtered.append(row)
    return filtered


def _build_export_rows(rows: list[dict[str, str]]) -> list[tuple[str, str]]:
    required_cols = ["Name", "Project Title"]
    if rows:
        missing = [col for col in required_cols if col not in rows[0]]
        if missing:
            raise ValueError(f"Missing required columns: {', '.join(missing)}")

    export_rows = {
        (_safe_str(row.get("Name")), _safe_str(row.get("Project Title")))
        for row in rows
        if _safe_str(row.get("Name")) or _safe_str(row.get("Project Title"))
    }
    return sorted(export_rows, key=lambda item: (item[0].lower(), item[1].lower()))


def _col_name(index: int) -> str:
    name = ""
    current = index
    while current > 0:
        current, rem = divmod(current - 1, 26)
        name = chr(65 + rem) + name
    return name


def _xml_escape(value: str) -> str:
    return (
        value.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace('"', "&quot;")
        .replace("'", "&apos;")
    )


def _inline_cell(ref: str, value: str) -> str:
    return (
        f'<c r="{ref}" t="inlineStr"><is><t>{_xml_escape(value)}</t></is></c>'
    )


def _build_sheet_xml(rows: list[tuple[str, str]]) -> str:
    all_rows = [("Project Leader Name", "Project Title"), *rows]
    xml_rows: list[str] = []
    for row_idx, row_values in enumerate(all_rows, start=1):
        cells = [
            _inline_cell(f"{_col_name(col_idx)}{row_idx}", _safe_str(value))
            for col_idx, value in enumerate(row_values, start=1)
        ]
        xml_rows.append(f'<row r="{row_idx}">{"".join(cells)}</row>')

    last_row = len(all_rows)
    dimension = f"A1:B{last_row}"
    return (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">'
        f'<dimension ref="{dimension}"/>'
        '<sheetViews><sheetView workbookViewId="0"/></sheetViews>'
        '<sheetFormatPr defaultRowHeight="15"/>'
        '<cols><col min="1" max="1" width="28" customWidth="1"/>'
        '<col min="2" max="2" width="50" customWidth="1"/></cols>'
        f'<sheetData>{"".join(xml_rows)}</sheetData>'
        '</worksheet>'
    )


def _write_xlsx(output_path: Path, rows: list[tuple[str, str]]) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    sheet_xml = _build_sheet_xml(rows)
    with ZipFile(output_path, "w", compression=ZIP_DEFLATED) as archive:
        archive.writestr(
            "[Content_Types].xml",
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
            '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
            '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
            '<Default Extension="xml" ContentType="application/xml"/>'
            '<Override PartName="/xl/workbook.xml" '
            'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>'
            '<Override PartName="/xl/worksheets/sheet1.xml" '
            'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>'
            '<Override PartName="/docProps/core.xml" '
            'ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>'
            '<Override PartName="/docProps/app.xml" '
            'ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>'
            '</Types>',
        )
        archive.writestr(
            "_rels/.rels",
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
            '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>'
            '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>'
            '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>'
            '</Relationships>',
        )
        archive.writestr(
            "docProps/app.xml",
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
            '<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" '
            'xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">'
            '<Application>Python</Application>'
            '</Properties>',
        )
        archive.writestr(
            "docProps/core.xml",
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
            '<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" '
            'xmlns:dc="http://purl.org/dc/elements/1.1/" '
            'xmlns:dcterms="http://purl.org/dc/terms/" '
            'xmlns:dcmitype="http://purl.org/dc/dcmitype/" '
            'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'
            '<dc:creator>Codex</dc:creator>'
            '<cp:lastModifiedBy>Codex</cp:lastModifiedBy>'
            '</cp:coreProperties>',
        )
        archive.writestr(
            "xl/workbook.xml",
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
            '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" '
            'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">'
            '<sheets><sheet name="Project Leaders" sheetId="1" r:id="rId1"/></sheets>'
            '</workbook>',
        )
        archive.writestr(
            "xl/_rels/workbook.xml.rels",
            '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
            '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>'
            '</Relationships>',
        )
        archive.writestr("xl/worksheets/sheet1.xml", sheet_xml)


def export_project_leaders(input_path: Path, output_path: Path, year: str | None = None) -> Path:
    rows = _load_rows(input_path)
    filtered_rows = _filter_year(rows, year)
    export_rows = _build_export_rows(filtered_rows)
    _write_xlsx(output_path, export_rows)
    return output_path


def main(argv: list[str]) -> int:
    args = _parse_args(argv)
    input_path = Path(args.input)
    output_path = Path(args.output)

    if not input_path.exists():
        print(f"Input not found: {input_path}")
        return 2

    try:
        written_path = export_project_leaders(input_path, output_path, args.year)
    except Exception as exc:
        print(f"Export failed: {exc}")
        return 1

    print(f"Excel written to: {written_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
