import numpy as np
import re

def parse_input(input_file: str):
    with open(input_file) as fh:
        input = fh.readlines()

    input = [[[int(n) for n in c.split(',')] for c in i.split(' -> ')] for i in input]

    return input

def solve(input_file: str, debug: bool = False):
    input = parse_input(input_file)

    inp = np.array(input)

    xs = inp[:, :, 0]
    ys = inp[:, :, 1]

    max_x = xs.max()
    max_y = ys.max()

    grid = np.zeros((max_x+1, max_y+1), dtype=int)

    for i in input:
        st, fi = i
        x0, y0 = st
        x1, y1 = fi
        if debug: 
            print(x0, y0, x1, y1, x0 == x1, y0 == y1, (x0 == x1 or y0 == y1))
        if (x0 == x1 or y0 == y1):
            xs = min(x0, x1)
            xf = max(x0, x1)
            ys = min(y0, y1)
            yf = max(y0, y1)
            grid[ys:yf+1, xs:xf+1] += 1

    if debug:
        print(grid)

    return (grid >= 2).sum()


print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=False))
