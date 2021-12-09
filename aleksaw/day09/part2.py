import numpy as np
import re

def parse_input(input_file: str):
    with open(input_file) as fh:
        inp = fh.readlines()
    
    inpu = [[int(c) for c in i.strip()] for i in inp]

    return np.array(inpu)

directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

def solve(input_file: str, debug: bool = False):
    inp = parse_input(input_file)

    mask = np.full_like(inp, False)

    for x in range(mask.shape[1]):
        for y in range(mask.shape[0]):
            num_higher = 0
            for dire in directions:
                if y+dire[1] < 0 or x+dire[0] < 0:
                    num_higher += 1
                    continue
                try:
                    if inp[y][x] < inp[y+dire[1],x+dire[0]]:
                        num_higher += 1
                except IndexError:
                    num_higher += 1
            if num_higher == 4:
                mask[y,x] = True

    basins = {
        (x, y): set()
        for x in range(mask.shape[1])
        for y in range(mask.shape[0])
        if mask[y,x]
    }
    if debug:
        print(basins)

    for coords, basin_members in basins.items():
        members = collect_basin_neighbours(coords[0], coords[1], basin_members, inp)
        if debug:
            print(members)
        basins[coords] = members

    sizes = [len(v) for v in basins.values()]
    if debug:
        print(sizes)

    selected = sorted(sizes)[-3:]

    assert len(selected) == 3

    return selected[0] * selected[1] * selected[2]

def collect_basin_neighbours(x: int, y: int, members: set, terrain: np.ndarray):
    y_max, x_max = terrain.shape
    # y_max -= 1
    # x_max -= 1

    for dire in directions:
        xc = x + dire[0]
        yc = y + dire[1]

        if xc < 0 or xc >= x_max or yc < 0 or yc >= y_max:
            continue

        c_height = terrain[yc, xc]
        if (xc, yc) not in members and c_height < 9:
            members.add((xc, yc))
            members = collect_basin_neighbours(xc, yc, members, terrain)

    return members


print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=False))
