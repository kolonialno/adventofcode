import numpy as np


def num_points_over_1(filename="inputs/05.txt", dim=1000, skip_diagonal=False) -> int:
    with open(filename) as f:
        grid = np.zeros([dim, dim])
        for line in f.readlines():
            # Parse input
            line = line.split()
            x1, y1 = map(int, line[0].split(","))
            x2, y2 = map(int, line[2].split(","))

            # Process input
            if skip_diagonal and (x1 != x2) and (y1 != y2):
                continue

            range_x = np.linspace(x1, x2, num=abs(x1 - x2) + 1, dtype=int)
            range_y = np.linspace(y1, y2, num=abs(y1 - y2) + 1, dtype=int)

            # Update grid
            grid[range_x, range_y] += 1

    return len(grid[grid > 1])


answer1 = num_points_over_1(skip_diagonal=True)
answer2 = num_points_over_1()

print(f"Answer1: {answer1}")  # 6007
print(f"Answer2: {answer2}")  # 19349
