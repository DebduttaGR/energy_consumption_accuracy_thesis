"""
Script: fetch_historical_weather.py
Fetches historical weather data from World Weather Online for a given airport/location and month.
Usage:
    python fetch_historical_weather.py --location "JFK" --year 2016 --month 1 --api_key YOUR_KEY

Outputs JSON to './JFK_2016-01.json'.
"""
import requests
import calendar
import argparse
import os

API_URL = 'http://api.worldweatheronline.com/premium/v1/past-weather.ashx'


def fetch_historical(api_key: str, location: str, year: int, month: int, output_dir: str = '.') -> str:
    """
    Fetch past weather data for given location and month.
    Returns the path to the saved JSON file.
    """
    # Compute date range
    start_date = f"{year}-{month:02d}-01"
    # last day of month
    _, last_day = calendar.monthrange(year, month)
    end_date = f"{year}-{month:02d}-{last_day:02d}"

    params = {
        'key': api_key,
        'q': location,
        'format': 'json',
        'date': start_date,
        'enddate': end_date,
        # you can add more optional parameters, e.g. 'tp': '24' to get daily
    }

    response = requests.get(API_URL, params=params)
    response.raise_for_status()
    data = response.json()

    # Prepare output filename
    fname = f"{location}_{year}-{month}.json"
    out_path = os.path.join(output_dir, fname)
    with open(out_path, 'w') as f:
        import json
        json.dump(data, f, indent=2)

    print(f"Saved historical weather for {location} {year}-{month:02d} to {out_path}")
    return out_path


def main():
    parser = argparse.ArgumentParser(description="Fetch historical weather data from World Weather Online")
    parser.add_argument('--api_key', required=True, help='Your World Weather Online API key')
    parser.add_argument('--location', default='JFK', help='Location query (e.g., JFK or lat,lon)')
    parser.add_argument('--year', type=int, default=2016, help='Year (e.g. 2016)')
    parser.add_argument('--month', type=int, default=1, help='Month number (1-12)')
    parser.add_argument('--output_dir', default='.', help='Directory to save JSON file')
    args = parser.parse_args()

    fetch_historical(
        api_key=args.api_key,
        location=args.location,
        year=args.year,
        month=args.month,
        output_dir=args.output_dir
    )


if __name__ == '__main__':
    main()
