import numpy as np
import re
import itertools

def parse_input(input_file: str):
    with open(input_file) as fh:
        input = fh.readlines()

    input = [(i.split(' | ')[0].split(), i.split(' | ')[1].split()) for i in input]

    return input

segment_in_digit = {
    'a': (0, 2, 3, 5, 6, 7, 8, 9),
    'b': (0, 4, 5, 6, 8, 9),
    'c': (0, 1, 2, 3, 4, 7, 8, 9),
    'd': (2, 3, 4, 5, 6, 8, 9),
    'e': (0, 2, 6, 8),
    'f': (0, 1, 3, 4, 5, 6, 7, 8, 9),
    'g': (0, 2, 3, 5, 6, 8, 9),
}

digit_segments = {}
for s, v in segment_in_digit.items():
    for d in v:
        # print(s, d, digit_segments)
        if d in digit_segments and digit_segments[d] is not None:
            digit_segments[d].append(s)
        else:
            digit_segments[d] = [s]

digit_map = {}
for k, v in digit_segments.items():
    a = np.array([c in v for c in 'abcdefg'])
    digit_map[k] = a



def solve(input_file: str, debug: bool = False):
    input = parse_input(input_file)

    decoded_values = []

    for line in input:
        fives = []
        sixes = []
        for v in line[0]:
            if len(v) == 2:
                one = v
            elif len(v) == 3:
                seven = v
            elif len(v) == 4:
                four = v
            elif len(v) == 7:
                eight = v
            elif len(v) == 5:
                fives.append(v)
            elif len(v) == 6:
                sixes.append(v)
        
        valid_permutations = []
        for i, p in enumerate(itertools.permutations('abcdefg')):
            if sorted(one) != sorted(''.join([c for c, y in zip(p, digit_map[1]) if y])):
                continue
            if sorted(seven) != sorted(''.join([c for c, y in zip(p, digit_map[7]) if y])):
                continue
            if sorted(four) != sorted(''.join([c for c, y in zip(p, digit_map[4]) if y])):
                continue
            
            pass_five = 0
            cands = [2, 3, 5]
            for five in fives:
                for cand in cands:
                    if sorted(five) == sorted(''.join([c for c, y in zip(p, digit_map[cand]) if y])):
                        pass_five += 1
                        cands.remove(cand)
                        break
            if pass_five < 3:
                continue
                
            pass_six = 0
            cands = [0, 6, 9]
            for six in sixes:
                for cand in cands:
                    if sorted(six) == sorted(''.join([c for c, y in zip(p, digit_map[cand]) if y])):
                        pass_six += 1
                        cands.remove(cand)
                        break
            if pass_six < 3:
                continue
                    
            
            valid_permutations.append(p)

        decoded = [np.array([c in d for c in valid_permutations[0]]) for d in line[1]]

        mul = np.array([1000, 100, 10, 1])
        vals = []
        for d in decoded:
            for v, mask in digit_map.items():
                if (d == mask).all():
                    vals.append(v)
        
        decoded_values.append((mul * np.array(vals)).sum())

    return sum(decoded_values)

print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=False))
