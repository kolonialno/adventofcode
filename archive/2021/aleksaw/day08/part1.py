import numpy as np
import re

def parse_input(input_file: str):
    with open(input_file) as fh:
        input = fh.readlines()

    input = [(i.split(' | ')[0].split(), i.split(' | ')[1].split()) for i in input]

    return input


def solve(input_file: str, debug: bool = False):
    input = parse_input(input_file)

    count = 0
    unique_lengths = {2, 3, 4, 7}
    for line in input:
        for digit in line[1]:
            if len(digit) in unique_lengths:
                count += 1

    return count

print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=False))
