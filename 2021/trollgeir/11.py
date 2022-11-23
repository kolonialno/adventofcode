import numpy as np

with open("inputs/11.txt") as f:
    grid = np.array([list(d) for d in f.read().splitlines()], dtype="float")

accumulated_flashes = 0
steps = 0

while True:
    steps += 1
    grid += 1
    while (grid > 9).any().any():
        for c in np.argwhere(grid > 9):  # get the coordinate (c) for flashers
            grid[
                max(c[0] - 1, 0) : min(c[0] + 2, 10),  # adjecents for flasher
                max(c[1] - 1, 0) : min(c[1] + 2, 10),
            ] += 1
            grid[c[0], c[1]] = np.nan  # Set flasher to nan to avoid further mutation

    all_flashers = np.isnan(grid)
    if all_flashers.all().all():
        answer2 = steps
        break
    accumulated_flashes += all_flashers.sum().sum()
    if steps == 100:
        answer1 = accumulated_flashes
    grid = np.nan_to_num(grid, 0)  # Set all flashed cells (nan) to zero

print(f"Answer1: {answer1}")  # 1603
print(f"Answer2: {answer2}")  # 222
