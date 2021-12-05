# import itertools
from pathlib import Path

import numpy as np
import pandas as pd

# Input parsing
p = Path("inputs/05.txt")
entries = [
    map(int, entry.split(","))
    for entry
    in p.read_text().strip().replace(" -> ", ",").split("\n")
]
entries = pd.DataFrame(
    entries,
    columns=["start_x", "start_y", "end_x", "end_y"],
)

# Matrix containing lines
max_dim = entries.to_numpy().max()
raster = np.zeros(
    (max_dim + 1, max_dim + 1),
    dtype=int,
)

for axis in ("x", "y"):
    entries[f"min_{axis}"] = entries[[f"start_{axis}", f"end_{axis}"]].min(axis=1)
    entries[f"max_{axis}"] = entries[[f"start_{axis}", f"end_{axis}"]].max(axis=1)

# Task 1
for _, row in entries.query("start_x == end_x or start_y == end_y").iterrows():
    raster[row.min_y:row.max_y + 1, row.min_x:row.max_x + 1] += 1

print((raster >= 2).sum())

# Task 2 
for axis in ("x", "y"):
    entries[f"{axis}_slope"] = np.sign(entries[f"end_{axis}"] - entries[f"start_{axis}"])

for _, row in entries.query("start_y != end_y and start_x != end_x").iterrows():
    raster[
        np.arange(start=row.start_y, stop=row.end_y + row.y_slope, step=row.y_slope),
        np.arange(start=row.start_x, stop=row.end_x + row.x_slope, step=row.x_slope),
    ] += 1

print((raster >= 2).sum())
