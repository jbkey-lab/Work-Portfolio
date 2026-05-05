[README.md](https://github.com/user-attachments/files/27411799/README.md)
# Work Portfolio

This repository is a portfolio of automation, reporting, data-processing, and web application projects built around agricultural research operations. The projects combine Python scripting, Excel processing, PDF generation, Flask web development, cloud data storage, and agronomy-specific workflows.

## Overview

The work in this repository focuses on practical tools that reduce manual administrative effort and turn raw operational data into usable reports, invoices, maps, and dashboards.

Core themes include:

- Automating repetitive research operations and billing tasks.
- Processing Excel, CSV, PDF, and form data into finished deliverables.
- Building tools for agricultural invoicing, spraying records, project reporting, and herbicide lookup workflows.
- Connecting field operations data with web interfaces, cloud storage, and generated documents.

## Technology Snapshot

- **Languages:** Python, JavaScript, HTML, CSS, PowerShell, shell scripting
- **Python libraries:** pandas, openpyxl, requests, reportlab, PyPDF2, pypdf, PyMuPDF, pdfrw, pikepdf, pyHanko
- **Web stack:** Flask, Jinja templates, JavaScript, CSS
- **Cloud/data services:** Azure Cosmos DB, Azure Blob Storage, Firebase Admin, Google Cloud Storage-style file workflows
- **Document workflows:** Excel workbooks, CSV exports, generated PDFs, fillable PDF forms, merged reports, image/document attachment handling

## Repository Map

| Area | Folder | Main Output |
| --- | --- | --- |
| Invoice automation | `AEAInvoice/` | Generated Excel invoices and summary CSV |
| Herbicide lookup | `Appril herbicide lookup/` | Matched APPRIL/EPA herbicide workbook |
| Web application | `PLIOP_Website-WebApp/` | Flask app, dashboards, invoices, and public project pages |
| Research reporting | `Project Request PDF/` | Merged project request/report PDFs |
| Spraying finance | `Spraying Reports/` | Excel/PDF reports and application map |
| Utility scripts | `Work Code/` | Small data and file-processing utilities |

## Featured Projects

### AEA Invoice Automation

**Folder:** `AEAInvoice/`

Automates creation and maintenance of AEA project invoice workbooks from project leader and billing data. The workflow reads structured Excel/CSV sources, filters records by year, generates project-specific invoice workbooks, updates acreage values, and produces a summary file for review.

**Highlights**

- Generates one invoice workbook per project leader using Excel templates.
- Handles Excel sheet name limits and duplicate project titles.
- Reads billing lookup data and calculates project totals from acreage and unit prices.
- Produces generated invoice artifacts and a `summary.csv` review output.
- Includes helper scripts to update acres and shorten project titles safely for Excel.

**Representative files**

- `AEAInvoice/aea_project_leader_export.py`
- `AEAInvoice/update_acres.py`
- `AEAInvoice/update_project_titles.py`
- `AEAInvoice/Generated_Invoices/summary.csv`

### APPRIL Herbicide Lookup

**Folder:** `Appril herbicide lookup/`

Command-line workflow for matching herbicide search terms against an APPRIL/EPA data dump. It normalizes product names, checks alternate names, and writes matched results back to Excel for review.

**Highlights**

- Searches both `PRODUCT_NAME` and `ABNS` alternate-name fields.
- Uses normalized exact phrase matching against a filtered EPA/APPRIL source.
- Produces a reviewed Excel output with best matches separated from possible-match columns.
- Includes a documented repeatable PowerShell run command.

**Representative files**

- `Appril herbicide lookup/appril_herbicide_lookup.py`
- `Appril herbicide lookup/APPRIL_herbicide_lookup_README.md`
- `Appril herbicide lookup/herbicide.xlsx`

### PLIOP Website and Web App

**Folder:** `PLIOP_Website-WebApp/`

Flask-based web application and personal project site. It includes public project pages, login-protected operational dashboards, invoice tooling, bulk file upload workflows, cloud data synchronization, PDF generation, and document filtering.

**Highlights**

- Flask app with Jinja templates for home, product, login, dashboard, documents, invoice, and project pages.
- Dashboard workflows for syncing data, syncing images, filtering records, downloading CSVs, and generating PDFs.
- Billing workflows for uploading Excel files, merging itemized bill data, updating cloud storage, and producing invoice PDFs.
- Integrates with Azure Cosmos DB, Azure Blob Storage, Firebase, and PDF-generation libraries.
- Includes project concepts for irrigation control, GPS RTK rover following, and plant sensors.

**Representative files**

- `PLIOP_Website-WebApp/PLIOP_app/views.py`
- `PLIOP_Website-WebApp/PLIOP_app/templates/index.html`
- `PLIOP_Website-WebApp/PLIOP_app/templates/invoice.html`
- `PLIOP_Website-WebApp/PLIOP_app/templates/about_me.html`
- `PLIOP_Website-WebApp/requirements.txt`
- `PLIOP_Website-WebApp/Dockerfile`
- `PLIOP_Website-WebApp/azure-pipelines.yml`

### Project Request and Research PDF Generation

**Folders:** `Project Request PDF/` and `Spraying Reports/`

Automates creation of AEA research project reporting packages. These scripts read submitted project data, download or locate attachments, convert different document formats to PDF, build title pages and tables of contents, merge PDFs, and add page numbering and clickable TOC links.

**Highlights**

- Reads project request data from CSV/XLSX.
- Exports project leader/project title summaries to Excel.
- Handles local and downloaded attachments.
- Converts Word, Excel, PowerPoint, CSV, and PDF attachments into report-ready PDFs.
- Builds merged research reports with title pages, TOC entries, page numbering, and attachment notices.

**Representative files**

- `Project Request PDF/aea_project_leader_export.py`
- `Project Request PDF/aea_research_report.py`
- `Spraying Reports/aea_project_leader_export.py`
- `Spraying Reports/aea_research_report.py`

### Spraying Financial Tracking and Mapping

**Folder:** `Spraying Reports/`

Turns SMS field operation exports and tank mix/product cost data into financial tracking outputs. The workflow includes generated Excel reports, a PDF financial report, and an HTML map artifact.

**Highlights**

- Processes spraying application CSV exports.
- Uses product and tank mix data to calculate application costs.
- Produces Excel and PDF financial tracking reports.
- Generates an HTML map for spraying application review.
- Includes SMS export instructions for repeatable data extraction.

**Representative files**

- `Spraying Reports/spray finance report.py`
- `Spraying Reports/products.csv`
- `Spraying Reports/SMSMixCost.xlsx`
- `Spraying Reports/Spraying_reports.xlsx`
- `Spraying Reports/Spraying_financial_tracking_report.pdf`
- `Spraying Reports/spraying_application_map.html`
- `Spraying Reports/SMS Export instructions.txt`

### Utility Scripts

**Folder:** `Work Code/`

Smaller one-off utilities for file extraction and agronomy data handling.

**Representative files**

- `Work Code/extractallfolders.py`
- `Work Code/guidencefilecode.py`
- `Work Code/mergeagronfile.py`

## Skills Demonstrated

- Python automation for operational workflows.
- Excel and CSV parsing, validation, transformation, and report generation.
- PDF form filling, PDF merging, attachment conversion, table-of-contents generation, and page numbering.
- Flask application development with templates, routes, authentication, dashboards, and file upload flows.
- Cloud-backed data workflows using Azure and Firebase-related services.
- Agricultural research operations domain knowledge, including project billing, spraying records, product lookup, and field reporting.

## Sharing Note

Some folders may contain generated invoices, research records, field operation exports, and other operational data. Before uploading this repository publicly, review generated Excel, CSV, and PDF files for personal, financial, research, or institutional information. Source code and sanitized sample inputs/outputs are safer to share than full production datasets.

## Summary

This portfolio demonstrates practical software development for agricultural operations: building data pipelines, automating Excel and PDF workflows, turning raw field and billing data into reports, and creating a Flask web application that connects operational workflows with cloud-backed storage and document generation.
