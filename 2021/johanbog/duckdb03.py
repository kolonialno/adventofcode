import duckdb
from utils import render
import pandas as pd
input = pd.read_table('test.txt',header=None,dtype=str)
input.columns=['numbers']
con = duckdb.connect(":memory:")
with open("sql03_2.sql", "r") as sql:
    print(con.execute(render(sql.read())).fetchall())
