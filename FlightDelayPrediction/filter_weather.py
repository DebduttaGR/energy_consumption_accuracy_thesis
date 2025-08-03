import pandas as pd
import os

# ── CONFIG ────────────────────────────────────────────────────────────────
BASE_DIR    = os.path.dirname(__file__)
DATA_DIR    = os.path.join(BASE_DIR, 'data')
WEATHER_CSV = os.path.join(DATA_DIR, 'weather.csv')
OUTPUT_CSV  = os.path.join(DATA_DIR, 'weather_filtered.csv')

# Desired fields to keep
DESIRED_FIELDS = [
    'WindSpeedKmph', 'WindDirDegree', 'WeatherCode',
    'precipMM', 'Visibility', 'Pressure', 'Cloudcover',
    'DewPointF', 'WindGustKmp', 'tempF', 'WindChillF', 'Humidity',
    'date', 'time', 'airport'
]

# Mapping substrings in station NAME → IATA code
AIRPORT_MAP = {
    'CHICAGO OHARE':       'ORD',
    'NEWARK LIBERTY':      'EWR',
    'ATLANTA HARTSFIELD':  'ATL',
    'DAL FTW':             'DFW',
    'HOUSTON INTERCONTINENTAL': 'IAH',
    'JFK INTERNATIONAL':   'JFK',
    'CHARLOTTE DOUGLAS':   'CLT',
    'MIAMI INTERNATIONAL': 'MIA',
    'PHOENIX SKY HARBOR':  'PHX',
    'ORLANDO INTERNATIONAL':'MCO',
    'LOS ANGELES INTERNATIONAL':'LAX',
    'DENVER INTERNATIONAL':'DEN',
    'SEATTLE TACOMA':      'SEA',
    'SAN FRANCISCO INTERNATIONAL':'SFO',
}

# ── UTILS ─────────────────────────────────────────────────────────────────

def map_airport(name: str) -> str:
    """
    Return the IATA code for a station NAME by checking key substrings.
    """
    upper = name.upper()
    for substr, code in AIRPORT_MAP.items():
        if substr in upper:
            return code
    return None

# ── MAIN PROCESS ───────────────────────────────────────────────────────────

def filter_weather_data(in_csv: str, out_csv: str) -> pd.DataFrame:
    # 1) Read raw daily summary
    df = pd.read_csv(in_csv)

    # 2) Map NAME → airport code
    df['airport'] = df['NAME'].apply(lambda n: map_airport(n) if isinstance(n, str) else None)

    # 3) Drop rows without matching airport
    df = df[df['airport'].notna()]

    # 4) Create desired fields where possible
    # WindSpeedKmph: AWND (m/s) * 3.6
    if 'AWND' in df.columns:
        df['WindSpeedKmph'] = df['AWND'] * 3.6

    # WindDirDegree: WDF2
    if 'WDF2' in df.columns:
        df['WindDirDegree'] = df['WDF2']

    # WeatherCode: first WTxx flag where value == 1
    wt_cols = [c for c in df.columns if c.startswith('WT')]
    if wt_cols:
        def extract_wt(row):
            for c in wt_cols:
                if row.get(c) == 1:
                    return int(c.replace('WT',''))
            return None
        df['WeatherCode'] = df.apply(extract_wt, axis=1)

    # precipMM: PRCP
    if 'PRCP' in df.columns:
        df['precipMM'] = df['PRCP']

    # Cloudcover: inverse of PSUN
    if 'PSUN' in df.columns:
        df['Cloudcover'] = 100 - df['PSUN']

    # tempF: TAVG (°C) to °F
    if 'TAVG' in df.columns:
        df['tempF'] = df['TAVG'] * 9/5 + 32

    # DewPointF: if DAPR exists as dewpoint? often no daily dewpoint
    # WindGustKmp: use WSF2*1.609 (mph→km/h)
    if 'WSF2' in df.columns:
        df['WindGustKmp'] = df['WSF2'] * 1.609

    # date and time columns
    df['date'] = df['DATE'] if 'DATE' in df.columns else None
    df['time'] = None

    # 5) Select only desired fields that exist
    out_cols = [c for c in DESIRED_FIELDS if c in df.columns]
    df_final = df[out_cols]

    # 6) Write to CSV
    df_final.to_csv(out_csv, index=False)
    return df_final

if __name__ == '__main__':
    print("Filtering weather data…")
    filtered = filter_weather_data(WEATHER_CSV, OUTPUT_CSV)
    print(f"Rows after filter: {len(filtered)}")
    print("Preview of first 10 rows:")
    print(filtered.head(10).to_string(index=False))
