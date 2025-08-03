import pandas as pd
import glob
import os

# Paths
BASE_DIR = os.path.dirname(__file__)
RAW_DIR = os.path.join(BASE_DIR, 'data', 'raw_zips')
LOOKUP_DIR = os.path.join(BASE_DIR, 'data')
COMBINED_CSV = os.path.join(LOOKUP_DIR, 'combined.csv')
FINAL_CSV = os.path.join(LOOKUP_DIR, 'filtered_flights.csv')

# Desired origin airport codes
desired_origins = [
    'ATL','CLT','DEN','DFW','EWR',
    'IAH','JFK','LAS','LAX','MCO',
    'MIA','ORD','PHX','SEA','SFO'
]


def combine_monthly_csvs(raw_dir: str = RAW_DIR, output_path: str = COMBINED_CSV) -> pd.DataFrame:
    """
    Combine all monthly on-time CSVs into a single DataFrame and save to CSV.
    """
    file_pattern = os.path.join(raw_dir, '201[6-7]_[0-1][0-9].csv')
    all_files = sorted(glob.glob(file_pattern))
    df_list = []
    for f in all_files:
        df = pd.read_csv(f)
        df_list.append(df)
    combined = pd.concat(df_list, ignore_index=True)
    combined.to_csv(output_path, index=False)
    print(f"Combined {len(df_list)} files into {output_path} ({combined.shape[0]} rows)")
    return combined


def add_origin_column(df: pd.DataFrame,
                      id_lookup_path: str = os.path.join(LOOKUP_DIR, 'L_AIRPORT_ID.csv'),
                      code_lookup_path: str = os.path.join(LOOKUP_DIR, 'L_AIRPORT.csv')) -> pd.DataFrame:
    """
    Read lookup tables, merge to map numeric OriginAirportID to IATA code;
    add 'Origin' column after 'ORIGIN_AIRPORT_ID'.
    """
    # Read lookup tables
    id_df = pd.read_csv(id_lookup_path, names=['AirportID', 'Description'], header=0)
    code_df = pd.read_csv(code_lookup_path, names=['IATA', 'Description'], header=0)
    # Merge on Description
    mapping = id_df.merge(code_df, on='Description', how='inner')
    id_to_iata = dict(zip(mapping['AirportID'], mapping['IATA']))

    # Map and insert column
    df['Origin'] = df['ORIGIN_AIRPORT_ID'].map(id_to_iata)
    cols = list(df.columns)
    idx = cols.index('ORIGIN_AIRPORT_ID')
    # remove and reinsert
    cols.remove('Origin')
    cols.insert(idx + 1, 'Origin')
    df = df[cols]
    print("Added 'Origin' column based on lookup mappings")
    return df


def filter_by_origins(df: pd.DataFrame, origins: list = desired_origins) -> pd.DataFrame:
    """
    Filter the DataFrame to only keep rows where 'Origin' is in the provided list.
    Save the filtered DataFrame to FINAL_CSV.
    """
    filtered = df[df['Origin'].isin(origins)].copy()
    filtered.to_csv(FINAL_CSV, index=False)
    print(f"Filtered data to {len(origins)} origins, resulting in {filtered.shape[0]} rows saved to {FINAL_CSV}")
    return filtered


if __name__ == '__main__':
    # Step 1: Combine all monthly CSVs
    combined_df = combine_monthly_csvs()
    # Step 2: Add Origin IATA codes
    combined_with_origin = add_origin_column(combined_df)
    # Step 3: Filter by desired origins
    filtered_df = filter_by_origins(combined_with_origin)
    print("\nFirst 50 rows of filtered data:\n")
    print(filtered_df.head(50).to_string(index=False))
