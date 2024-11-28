import numpy as np
import re

def parse_input(input_file: str):
    with open(input_file) as fh:
        input = fh.readlines()

    input = [int(i) for i in input[0].split(',')]

    return input

def cost(dist):
    return dist * (dist+1) / 2

def solve(input_file: str, debug: bool = False):
    input = parse_input(input_file)
    if debug:
        print(input)

    i = np.array(input)

    pos = np.mean(i).round()
    if debug:
        print(np.mean(i))
        print(pos)

    c = cost(np.abs(i - pos)).sum()
    cl = cost(np.abs(i - (pos-1))).sum()
    ch = cost(np.abs(i - (pos+1))).sum()
    if debug:
        print(c, cl, ch)
    cp = c
    if ch < c:
        dir = 1
        c = ch
    elif cl < c:
        dir = -1
        c = cl
    else:
        return c

    if debug:
        print(c, cp)
    pos += dir
    while c < cp:
        pos += dir
        cp = c
        c = cost(np.abs(i - pos)).sum()

    if debug:
        print(pos-dir)
    return cp

print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=True))
