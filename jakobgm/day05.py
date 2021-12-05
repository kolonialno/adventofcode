# import itertools
from pathlib import Path

import numpy as np
import pandas as pd

# Input parsing
p = Path("inputs/05.txt")
entries = [entry.split(" -> ") for entry in p.read_text().strip().split("\n")]
entries = [
    [list(map(int, start.split(","))) + list(map(int, end.split(",")))][0]
    for start, end
    in entries
]
entries = pd.DataFrame(entries, columns=["start_x", "start_y", "end_x", "end_y"])

# Matrix containing lines
max_dim = entries.to_numpy().max()
m = np.zeros((max_dim + 1, max_dim + 1), dtype=int)

# Task 1
for _, row in entries.query("start_x == end_x").iterrows():
    start_y, end_y = min(row.start_y, row.end_y), max(row.start_y, row.end_y)
    m[start_y:end_y + 1, row.start_x] += 1

for _, row in entries.query("start_y == end_y").iterrows():
    start_x, end_x = min(row.start_x, row.end_x), max(row.start_x, row.end_x)
    m[row.start_y, start_x:end_x + 1] += 1

print((m >= 2).sum())

# Task 2 
for _, row in entries.query("start_y != end_y and start_x != end_x").iterrows():
    x_delta = np.sign(row.end_x - row.start_x)
    y_delta = np.sign(row.end_y - row.start_y)
    x, y = row.start_x, row.start_y
    while x != (row.end_x + x_delta) and y != (row.end_y + y_delta):
        m[y, x] += 1
        x += x_delta
        y += y_delta

print((m >= 2).sum())
