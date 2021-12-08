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
        x_dir = 0 if x1==x0 else (x1-x0) // abs(x1-x0)
        y_dir = 0 if y1==y0 else (y1-y0) // abs(y1-y0)
        x, y = x0, y0
        cont = True
        while cont:
            if debug:
                print(x, y)
            grid[y,x] += 1
            if x_dir != 0 and x == x1:
                cont = False
            elif y_dir != 0 and y == y1:
                cont = False
            else:
                x += x_dir
                y += y_dir

    if debug:
        print(grid)

    return (grid >= 2).sum()


print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=False))
