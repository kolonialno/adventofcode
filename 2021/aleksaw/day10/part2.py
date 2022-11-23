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
                return False
        else:
            print(f"- {line} - Expected char in {closing_chars}, but found {c} instead")
            return 'err'

    missing_closers = [closing_chars[opening_chars.index(o)] for o in openers[::-1]]

    if debug:
        print(f"- {line} - Complete by adding {''.join(missing_closers)}")

    return missing_closers

def solve(input_file: str, debug: bool = False):
    inp = parse_input(input_file)

    corrupted = np.full(len(inp), False)
    closers = []
    for i, line in enumerate(inp):
        ret = check_line(line, debug)
        if ret and ret != 'err':
            closers.append(ret)
        elif not ret:
            corrupted[i] = True


    scores = {
        ')': 1,
        ']': 2,
        '}': 3,
        '>': 4
    }

    score_list = [0 for _ in closers]
    for i, cs in enumerate(closers):
        for c in cs:
            score_list[i] *= 5
            score_list[i] += scores[c]
    if debug:
        print(score_list)

    score = np.median(np.array(score_list, dtype=int))

    return score

print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=False))
