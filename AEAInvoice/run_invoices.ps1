# Set the base directory for your AEAInvoice files
$AEA_DIR = "C:\Users\jblamkey\Documents\AEAInvoice"

# Configuration
$YEAR = "2026"
$INPUT_EXT = "xlsx" # Can be csv or xlsx

$INPUT_FORM = "$AEA_DIR\AEA_$($YEAR)_Research_Project_Form.$INPUT_EXT"
$BILLS_FILE = "$AEA_DIR\bills.xlsx"
$TEMPLATE_FILE = "$AEA_DIR\PI_Year_AEA.xlsx"
$OUTPUT_DIR = "$AEA_DIR\Generated_Invoices"

# Run the script
python "$AEA_DIR\aea_project_leader_export.py" `
    --input "$INPUT_FORM" `
    --bills "$BILLS_FILE" `
    --template "$TEMPLATE_FILE" `
    --output "$OUTPUT_DIR" `
    --year "$YEAR"

Write-Host "Invoice generation complete! Files are in: $OUTPUT_DIR"