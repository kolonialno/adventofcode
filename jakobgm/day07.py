from pathlib import Path

import numpy as np

# Input parsing
p = Path("inputs/07.txt")
positions = np.array([
    int(position)
    for position
    in p.read_text().strip().split(",")
])

# Task 1
min_fuel = np.inf
for position in range(positions.min(), positions.max() + 1):
    fuel = np.abs(positions - position).sum()
    if fuel < min_fuel:
        min_fuel = fuel
        min_position = position
print(min_fuel, min_position)

# Task 2
min_fuel = np.inf
for position in range(positions.min(), positions.max() + 1):
    # Linear distance
    d = np.abs(positions - position)
    # Sum of numbers between 1 and n = n * (n + 1) / 2
    fuel = 0.5 * (d * (d + 1)).sum()
    if fuel < min_fuel:
        min_fuel = fuel
        min_position = position

print(min_fuel, min_position)
