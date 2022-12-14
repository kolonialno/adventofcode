import numpy as np


S = (500, 0)
rock_paths = []

xmin, xmax = (S[0],) * 2
ymin, ymax = (S[1],) * 2

with open("input.txt") as f:
    for l in f.read().splitlines():
        rock_path = []
        for p in l.split(" -> "):
            p1, p2 = p.split(",")
            x, y = (int(p1), int(p2))
            xmin = min(xmin, x)
            xmax = max(xmax, x)
            ymin = min(ymin, y)
            ymax = max(ymax, y)
            rock_path.append((x, y))
        rock_paths.append(rock_path)


nx = int(ymax + 3) * 2 + 1
ny = int(ymax) + 3

x_offset = (500 - nx // 2 + 1)

grid = np.zeros((ny, nx))

for rock_path in rock_paths:

    for i in range(len(rock_path) - 1):
        start, end = rock_path[i], rock_path[i + 1]
        xstart = start[0] - x_offset
        xend = end[0] - x_offset
        xstart, xend = min(xstart, xend), max(xstart, xend) + 1

        ystart = start[1] - int(ymin)
        yend = end[1] - int(ymin)
        ystart, yend = min(ystart, yend), max(ystart, yend) + 1

        grid[ystart : yend, xstart : xend] = 1

grid[ymax + 2,:] = 1

startpoint = (S[0] - x_offset, S[1] - int(ymin))
moving_sandcorn = startpoint
units = 0

try:
    while True:
        # Try down
        # print(grid)
        if grid[moving_sandcorn[1] + 1, moving_sandcorn[0]] == 0:
            moving_sandcorn = (moving_sandcorn[0], moving_sandcorn[1] + 1)
        # Down left
        elif grid[moving_sandcorn[1] + 1, moving_sandcorn[0] - 1] == 0:
            moving_sandcorn = (moving_sandcorn[0] - 1, moving_sandcorn[1] + 1)
        # Down right
        elif grid[moving_sandcorn[1] + 1, moving_sandcorn[0] + 1] == 0:
            moving_sandcorn = (moving_sandcorn[0] + 1, moving_sandcorn[1] + 1)
        # Comes to rest
        else:
            units += 1
            if moving_sandcorn == startpoint:
                raise IndexError
            grid[moving_sandcorn[1], moving_sandcorn[0]] = 2
            moving_sandcorn = startpoint
except IndexError:
    print(units)
