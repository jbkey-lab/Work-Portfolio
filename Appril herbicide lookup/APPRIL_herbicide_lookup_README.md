# APPRIL herbicide lookup

This script reads `C:\Users\jblamkey\Documents\herbicide.xlsx`, uses the `Search Term` column, and matches against:

- `PRODUCT_NAME`
- `ABNS` alternate names

The current workflow uses exact normalized phrase matching against the filtered EPA dump:

- `C:\Users\jblamkey\Documents\apprildatadump_public_Filtered.xlsx`

## Script

- `C:\Users\jblamkey\Documents\appril_herbicide_lookup.py`

## Run

```powershell
& "C:\Users\jblamkey\AppData\Local\Programs\Python\Python311\python.exe" `
  "C:\Users\jblamkey\Documents\appril_herbicide_lookup.py" `
  "C:\Users\jblamkey\Documents\herbicide.xlsx" `
  --source dump `
  --appril-dump "C:\Users\jblamkey\Documents\apprildatadump_public_Filtered.xlsx"
```

## Output

- `C:\Users\jblamkey\Documents\herbicide_appril_matches.xlsx`

## Notes

- The script searches only the literal `Search Term` value from each row.
- It checks both the primary product name and alternate names from `ABNS`.
- The `Best Matches` sheet excludes the rolled-up possible-match columns.
