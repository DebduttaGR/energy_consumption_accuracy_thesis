from utils import connect_db

if __name__ == "__main__":
    print(connect_db("SELECT count(*) FROM icustays;"))
