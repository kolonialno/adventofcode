import numpy as np
import re

def parse_input(input_file: str):
    with open(input_file) as fh:
        input = fh.readlines()

    input = [int(i) for i in input[0].split(',')]

    return input


def solve(input_file: str, debug: bool = False):
    input = parse_input(input_file)
    if debug:
        print(input)

    i = np.array(input)

    optimal = np.median(i).round()
    if debug:
        print(np.median(i))
        print(optimal)

    cost = np.abs(i - optimal).sum()

    return cost

print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=False))
