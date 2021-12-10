import numpy as np
import re

def parse_input(input_file: str):
    with open(input_file) as fh:
        inp = fh.readlines()

    return [i.strip() for i in inp]

def check_line(line, debug):
    opening_chars = ['(', '[', '{', '<']
    closing_chars = [')', ']', '}', '>']
    openers = []
    for c in line:
        if c in opening_chars:
            openers.append(c)
        elif c in closing_chars:
            opener = openers.pop()
            closer = closing_chars[opening_chars.index(opener)]
            if closer != c:
                if debug:
                    print(f"- {line} - Expected {closer}, but found {c} instead")
                return c
        else:
            print(f"- {line} - Expected char in {closing_chars}, but found {c} instead")
            return 'err'

def solve(input_file: str, debug: bool = False):
    inp = parse_input(input_file)

    invalid = np.full(len(inp), False)
    corrupted = []
    for i, line in enumerate(inp):
        ret = check_line(line, debug)
        if ret is not None:
            invalid[i] = True
            corrupted.append(ret)

    scores = {
        ')': 3,
        ']': 57,
        '}': 1197,
        '>': 25137
    }

    score = 0
    for c in corrupted:
        if c != 'err':
            score += scores[c]

    return score

print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=False))
