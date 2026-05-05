import os
import shutil

# Root directory where the files currently are
root_dir = r"C:\Users\jblamkey\Documents\Guidence lines"

# Loop through every file
for filename in os.listdir(root_dir):
    if not filename.lower().endswith((".shp", ".shx", ".dbf", ".prj")):
        continue  # ignore unrelated files

    # Split filename into parts
    parts = filename.split("_")
    if len(parts) < 6:
        continue  # unexpected naming format, skip

    farm_name      = parts[0] + " " + parts[1] + " " + parts[2]        # "ISU Research Farms"
    field_name     = parts[3]                                         # "Sorenson"
    field_number   = parts[4]                                         # "4"

    year           = parts[5]                                         # "2024"
    product        = parts[6]                                         # "NO Product" → needs replacing
    rest           = "_".join(parts[7:])                              # the rest of the filename

    # Replace "NO Product" with "Guidance"
    product = product.replace("NO Product", "Guidance")

    # Rebuild the full filename with the corrected product
    new_filename = f"{farm_name.replace(' ', '_')}_{field_name}_{field_number}_{year}_{product}_{rest}"

    # Build destination folders
    farm_folder = os.path.join(root_dir, farm_name)
    field_folder = os.path.join(farm_folder, field_name)
    number_folder = os.path.join(field_folder, field_number)

    # Create required folders
    os.makedirs(number_folder, exist_ok=True)

    # Full paths
    src_path = os.path.join(root_dir, filename)
    dst_path = os.path.join(number_folder, new_filename)

    print(f"Moving:\n  {src_path}\n→ {dst_path}")

    # Move file
    shutil.move(src_path, dst_path)

print("Done organizing shapefiles.")
