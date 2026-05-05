from collections import defaultdict
import pandas as pd
import os

def group_operations_by_field(df, allowed_operations):
    grouped_operations = defaultdict(list)

    # Filter and clean allowed operations
    df_filtered = df[df["Field Operation"].isin(allowed_operations)].copy()
    df_filtered["Field Operation"] = df_filtered["Field Operation"].str.strip()

    # Convert date column to datetime and sort
    df_filtered["Date of Operation"] = pd.to_datetime(df_filtered["Date of Operation"], errors="coerce")
    df_filtered = df_filtered.sort_values(by=["Grower", "Farm", "Field", "Date of Operation"])

    for _, row in df_filtered.iterrows():
        key = (row["Grower"], row["Farm"], row["Field"])
        grouped_operations[key].append(row["Field Operation"])

    final_reports = []

    for key, ops in grouped_operations.items():
        seen = set()
        unique_ops = [op for op in ops if not (op in seen or seen.add(op))]

        report = {
            "Grower": key[0],
            "Farm": key[1],
            "Field": key[2],
        }

        for i, op in enumerate(unique_ops):
            report[f"Tillage Operation{i+1}"] = op

        final_reports.append(report)

    return pd.DataFrame(final_reports)

# File paths
main_folder = os.path.join(r"C:\Users\jblamkey\Downloads")
agronomic_path = os.path.join(main_folder, "Agronomic Information(Sheet1).csv")
gis_path = os.path.join(main_folder, "GIS_cropsForFields (15).csv")
output_path = os.path.join(main_folder, "GIS_cropsForFields.csv")

# Load and clean CSVs
agronomic_df = pd.read_csv(agronomic_path, encoding="latin1")
gis_df = pd.read_csv(gis_path)

agronomic_df.columns = agronomic_df.columns.str.strip()
agronomic_df["Field Operation"] = agronomic_df["Field Operation"].str.strip()
gis_df.columns = gis_df.columns.str.strip()

# Allowed operations
allowed_operations = [
    "Chisel Plow", "Deep rip. JD 2720", "Disk", "Disk Ripping",
    "Field Cultivate", "Inter-Field Cultivate", "Moldboard plow",
    "Rake rye", "roll chop cornstalks", "Buffalo No-Till Cultivator",
    "Stalk Roller Chop", "Strip Till"
]

# Group and merge
operations_df = group_operations_by_field(agronomic_df, allowed_operations)
final_df = pd.merge(gis_df, operations_df, on=["Grower", "Farm", "Field"], how="left")
final_df.to_csv(output_path, index=False)
