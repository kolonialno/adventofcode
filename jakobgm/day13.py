from pathlib import Path

import matplotlib.pyplot as plt

import numpy as np

puzzle = Path("inputs/13.txt").read_text().strip()
dots, folds = puzzle.split("\n\n")
dots = [tuple(map(int, l.split(","))) for l in dots.split("\n")]

grid = np.zeros(
    (max(dots, key=lambda x: x[1])[1] + 1, max(dots, key=lambda x: x[0])[0] + 1) ,
    dtype=bool,
)
for x, y in dots:
    grid[y, x] = True

for fold in folds.split("\n"):
    instruction, split = fold.split("=")
    split = int(split)
    if instruction[-1] == "x":
        split_length = grid.shape[1] - split
        grid[:, :split] = np.logical_or(
            np.flip(grid[:, split + 1:], 1),
            grid[:, :split],
        )
        grid = np.delete(grid, range(split, grid.shape[1]), 1)
    else:
        split_length = grid.shape[0] - split
        grid[:split, :] = np.logical_or(
            np.flip(grid[split + 1:, :], 0),
            grid[:split, :],
        )
        grid = np.delete(grid, range(split, grid.shape[0]), 0)

plt.imshow(grid)
plt.savefig("18.png")
