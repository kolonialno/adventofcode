import duckdb
from utils import render

con = duckdb.connect(":memory:")
with open("sql03.sql", "r") as sql:
    print(con.execute(render(sql.read())).fetchone())
