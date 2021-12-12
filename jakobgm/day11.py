from pathlib import Path
from itertools import count, product

import numpy as np

energy = np.array([
    list(map(int, row))
    for row
    in Path("inputs/11.txt").read_text().strip().split()
], dtype=float)
energy = np.pad(energy, pad_width=1, mode="constant", constant_values=-np.inf)

total_flashes = 0
for step in count(start=1):
    energy += 1
    flash_stack = set(zip(*np.where(energy > 9)))
    has_flashed = set()

    while flash_stack:
        row, col = flash_stack.pop()
        has_flashed.add((row, col))

        shifts = (
            (row + s[0], col + s[1])
            for s
            in product([-1, 0, 1], [-1, 0, 1])
            if s != (0, 0)
        )
        for row_shift, col_shift in shifts:
            energy[row_shift, col_shift] += 1
            if (
                energy[row_shift, col_shift] > 9
                and (row_shift, col_shift) not in has_flashed
            ):
                flash_stack.add((row_shift, col_shift))

    total_flashes += len(has_flashed)
    for row, col in has_flashed:
        energy[row, col] = 0

    if step == 100:
        print(total_flashes)
    if len(has_flashed) == 100:
        print(step)
        break
