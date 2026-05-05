import os
import zipfile

# Path to your folder
base_path = r"C:\Users\jblamkey\Downloads\requestedFiles (5)"

# Loop through all files
for filename in os.listdir(base_path):
    if filename.lower().endswith(".zip"):
        zip_path = os.path.join(base_path, filename)
        extract_folder = os.path.join(base_path, filename[:-4])  # remove .zip

        print(f"Extracting: {filename} → {extract_folder}")

        # Create output folder if missing
        os.makedirs(extract_folder, exist_ok=True)

        # Extract
        with zipfile.ZipFile(zip_path, 'r') as zip_ref:
            zip_ref.extractall(extract_folder)

print("✔ Done extracting all zip files!")
