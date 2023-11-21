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


def print_num_units(rock_paths, with_floor=False):

    if with_floor:
        nx = int(ymax + 3) * 2 + 1
        ny = int(ymax) + 3
        x_offset = 500 - nx // 2 + 1
    else:
        nx = int(ymax) + 1
        ny = int(ymax) + 1
        x_offset = xmin

    grid = np.zeros((ny, nx))

    for rock_path in rock_paths:

        for i in range(len(rock_path) - 1):
            start, end = rock_path[i], rock_path[i + 1]
            xstart, xend = start[0] - x_offset, end[0] - x_offset
            xstart, xend = min(xstart, xend), max(xstart, xend) + 1

            ystart, yend = start[1] - int(ymin), end[1] - int(ymin)
            ystart, yend = min(ystart, yend), max(ystart, yend) + 1

            grid[ystart:yend, xstart:xend] = 1

    if with_floor:
        grid[ymax + 2, :] = 1

    startpoint = (S[0] - x_offset, S[1] - int(ymin))
    moving_sandcorn = startpoint
    units = 0

    try:
        while True:
            if grid[moving_sandcorn[1] + 1, moving_sandcorn[0]] == 0:
                moving_sandcorn = (moving_sandcorn[0], moving_sandcorn[1] + 1)
            elif grid[moving_sandcorn[1] + 1, moving_sandcorn[0] - 1] == 0:
                moving_sandcorn = (moving_sandcorn[0] - 1, moving_sandcorn[1] + 1)
            elif grid[moving_sandcorn[1] + 1, moving_sandcorn[0] + 1] == 0:
                moving_sandcorn = (moving_sandcorn[0] + 1, moving_sandcorn[1] + 1)
            else:
                units += 1
                if moving_sandcorn == startpoint:
                    raise IndexError
                grid[moving_sandcorn[1], moving_sandcorn[0]] = 2
                moving_sandcorn = startpoint
    except IndexError:
        print(units)


print_num_units(rock_paths, with_floor=False)
print_num_units(rock_paths, with_floor=True)
