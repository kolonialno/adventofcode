from pathlib import Path

import polars as pl

problem = Path("input/1.txt").read_text()

max_calories = float("-inf")
rows = []
for elf_number, elf in enumerate(problem.strip().split("\n\n")):
    items = []
    for item in elf.strip().split("\n"):
        rows.append({"elf": elf_number, "calories": int(item)})

df = pl.DataFrame(rows)
print(df.groupby("elf").agg(pl.col("calories").sum()).select(pl.col("calories").max()))
print(
    df.groupby("elf")
    .agg(pl.col("calories").sum())
    .filter(pl.col("calories").rank("ordinal", reverse=True) <= 3)
    .select(pl.col("calories").sum())
)
