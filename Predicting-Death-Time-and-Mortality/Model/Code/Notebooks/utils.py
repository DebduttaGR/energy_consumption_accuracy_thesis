import os
import dotenv
import psycopg2
import pandas as pd

def connect_db(query):
    dotenv.load_dotenv(".env")
    #load_dotenv(dotenv_path=".env")
    # remove any pre‐existing USER to avoid fallback
    os.environ.pop("USER", None)

    user = os.environ["DB_USER"]
    host = os.environ["HOST"]
    port = int(os.environ["PORT"])
    passwd = os.environ["PASSWORD"]
    db = os.environ["DATABASE"]
    con = psycopg2.connect(user=user, password=passwd, host=host, port=port, database=db)
    result = pd.read_sql(query, con=con)
    con.close()
    return result
