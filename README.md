# Work Portfolio

This repository is a portfolio of automation, reporting, machine learning, breeding analytics, data-processing, and web application projects built around agricultural research operations. The projects combine Python, R, Excel processing, PDF generation, Flask web development, cloud data storage, and agronomy-specific workflows.

## Overview

The work in this repository focuses on practical tools that reduce manual administrative effort, support plant breeding and field research decisions, and turn raw operational data into usable reports, invoices, maps, dashboards, and model outputs.

Core themes include:

- Automating repetitive research operations and billing tasks.
- Processing Excel, CSV, PDF, image, and form data into finished deliverables.
- Building tools for agricultural invoicing, spraying records, project reporting, herbicide lookup, breeding values, and genomic prediction workflows.
- Connecting field operations data with web interfaces, cloud storage, generated documents, and statistical models.

## Technology Snapshot

- **Languages:** Python, R, JavaScript, HTML, CSS, PowerShell, shell scripting
- **Python libraries:** pandas, openpyxl, requests, reportlab, PyPDF2, pypdf, PyMuPDF, pdfrw, pikepdf, pyHanko
- **R/statistical tools:** tidyverse, data.table, lme4, caret, caretEnsemble, xgboost, BGLR-style genomic selection workflows, ASReml-based breeding analysis workflows
- **Machine learning:** XGBoost, gradient boosting, deep neural networks, CNN/Keras examples, quantile regression, genomic selection scripts
- **Web stack:** Flask, Jinja templates, JavaScript, CSS
- **Cloud/data services:** Azure Cosmos DB, Azure Blob Storage, Firebase Admin, Google Cloud Storage-style file workflows
- **Document workflows:** Excel workbooks, CSV exports, generated PDFs, fillable PDF forms, merged reports, image/document attachment handling

## Repository Map

| Area | Folder | Main Output |
| --- | --- | --- |
| Invoice automation | `AEAInvoice/` | Generated Excel invoices and summary CSV |
| Herbicide lookup | `Appril herbicide lookup/` | Matched APPRIL/EPA herbicide workbook |
| Corn breeding analytics | `Corn Breeding BreedValues/` | R package/functions for breeding values, spatial analysis, pedigree handling, and prediction |
| Machine learning scripts | `Machine Learning Scripts/` | R scripts and notebooks for genomic prediction, image classification, DNN/CNN models, and competition-style modeling |
| Web application | `PLIOP_Website-WebApp/` | Flask app, dashboards, invoices, and public project pages |
| Research reporting | `Project Request PDF/` | Merged project request/report PDFs |
| Spraying finance | `Spraying Reports/` | Excel/PDF reports and application map |
| Utility scripts | `Work Code/` | Small data and file-processing utilities |

## Featured Projects

### Corn Breeding BreedValues

**Folder:** `Corn Breeding BreedValues/BreedStats-main/`

`BreedStats` is an R package for breeding statistics, genetic value estimation, spatial analysis, pedigree handling, and prediction workflows. The package is structured with R package metadata, example scripts, bundled data, documentation, and reusable functions.

**Highlights**

- Implements breeding value workflows for corn breeding and hybrid/inbred evaluation.
- Includes ASReml-oriented spatial and breeding value analysis scripts.
- Provides tools for pedigree adjustment, genotype conversion, inbred name handling, image database support, and alpha lattice/yield trial analysis.
- Includes prediction scripts using XGBoost and mixed model style workflows.
- Provides simulated or packaged example data for testing package functions.

**Representative files**

- `Corn Breeding BreedValues/BreedStats-main/DESCRIPTION`
- `Corn Breeding BreedValues/BreedStats-main/R/asremlBV.R`
- `Corn Breeding BreedValues/BreedStats-main/R/xgboostBV.R`
- `Corn Breeding BreedValues/BreedStats-main/R/PedigreeEngine.R`
- `Corn Breeding BreedValues/BreedStats-main/R/Spatial Analysis_ASReml.R`
- `Corn Breeding BreedValues/BreedStats-main/R/YT_Alpha Lattice.R`
- `Corn Breeding BreedValues/BreedStats-main/Examples/`

### Machine Learning Scripts

**Folder:** `Machine Learning Scripts/scripts-main/`

A collection of R scripts, R Markdown files, and Jupyter notebooks exploring machine learning methods across crop modeling, genomic selection, image classification, sports prediction, finance-style prediction, and Kaggle-style modeling workflows.

**Highlights**

- Genomic selection and breeding value scripts using BGLR-style workflows, custom GS logic, XGBoost, quantile random forests, and deep neural networks.
- Keras/CNN examples for image classification and crop challenge modeling.
- R and notebook examples for gradient boosting, caret workflows, DNNs, CNNs, and quantile regression.
- Broader modeling practice examples, including March Madness, NFL, Jane Street, Jigsaw, and OSIC pulmonary fibrosis notebooks.
- Useful as a technical archive showing experimentation across model families and data types.

**Representative files**

- `Machine Learning Scripts/scripts-main/BGLR_GS.R`
- `Machine Learning Scripts/scripts-main/Custom_GS.R`
- `Machine Learning Scripts/scripts-main/xgboostBV.R`
- `Machine Learning Scripts/scripts-main/QRFBreedingValues.R`
- `Machine Learning Scripts/scripts-main/Breeding Values-DNN.R`
- `Machine Learning Scripts/scripts-main/cnn-keras.R`
- `Machine Learning Scripts/scripts-main/cropchallenge keras.ipynb`
- `Machine Learning Scripts/scripts-main/image_classification_from_scratch.ipynb`

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
- R package development and breeding statistics workflows.
- Genomic prediction, breeding value estimation, spatial analysis, pedigree handling, and yield trial analysis.
- Machine learning experimentation with XGBoost, caret, DNNs, CNNs, Keras, quantile regression, and competition-style notebooks.
- Excel and CSV parsing, validation, transformation, and report generation.
- PDF form filling, PDF merging, attachment conversion, table-of-contents generation, and page numbering.
- Flask application development with templates, routes, authentication, dashboards, and file upload flows.
- Cloud-backed data workflows using Azure and Firebase-related services.
- Agricultural research operations domain knowledge, including project billing, spraying records, product lookup, field reporting, and breeding analytics.

## Suggested Cleanup Before Uploading to GitHub

Before making this repository public, review and consider removing or sanitizing:

- Generated invoices, research records, field operation exports, and institutional datasets.
- Large generated files such as PDFs, Excel outputs, notebook checkpoints, `.rda`, `.rds`, `.npy`, `.zip`, and rendered `.html` notebooks when they are not needed for review.
- Cache and local-history files such as `__pycache__/`, `.ipynb_checkpoints/`, `.Rhistory`, and `.DS_Store`.
- API keys, connection strings, Firebase/Azure credentials, environment files, and private package files.
- Any data containing personal, financial, research, or institutional information.

Source code, documentation, and small sanitized sample inputs/outputs are safer to share than complete production datasets.

## Summary

This portfolio demonstrates practical software development for agricultural operations and breeding analytics: building data pipelines, automating Excel and PDF workflows, modeling breeding and field data, turning raw operational records into reports, and creating a Flask web application that connects operational workflows with cloud-backed storage and document generation.
