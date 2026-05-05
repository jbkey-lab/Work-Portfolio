import pandas as pd
import re
from pathlib import Path

def smart_shorten(title, limit=31):
    """Shorten title to limit, ensuring it ends on a whole word and is Excel-safe."""
    if not title or pd.isna(title):
        return "Project"
    
    # Remove characters forbidden in Excel sheet names: \ / * ? : [ ]
    clean_title = re.sub(r'[\\/*?:\[\]]', "", str(title)).strip()
    
    if len(clean_title) <= limit:
        return clean_title
        
    # Truncate and find the last space to avoid cutting words in half
    cut = clean_title[:limit]
    last_space = cut.rfind(" ")
    return cut[:last_space].rstrip() if last_space > 0 else cut

def update_source_file():
    file_path = Path(r"c:\Users\jblamkey\Documents\AEAInvoice\aea_2026_research_project_form.xlsx")
    if not file_path.exists():
        print(f"File not found: {file_path}")
        return

    # Load the data
    df = pd.read_excel(file_path)
    
    # Find the project title column (case-insensitive)
    title_col = next((c for c in df.columns if c.lower() == "project title"), None)
    
    if title_col:
        print(f"Generating 'short project title' from '{title_col}'...")
        df['short project title'] = df[title_col].apply(lambda x: smart_shorten(x, 31))
        df.to_excel(file_path, index=False)
        print("Successfully updated the Excel file with the new column.")
    else:
        print("Could not find a 'project title' column in the file.")

if __name__ == "__main__":
    update_source_file()