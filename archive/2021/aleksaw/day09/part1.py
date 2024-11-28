import numpy as np
import re

def parse_input(input_file: str):
    with open(input_file) as fh:
        inp = fh.readlines()
    
    inpu = [[int(c) for c in i.strip()] for i in inp]

    return np.array(inpu)



def solve(input_file: str, debug: bool = False):
    inp = parse_input(input_file)

    mask = np.full_like(inp, False)

    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for x in range(mask.shape[1]):
        for y in range(mask.shape[0]):
            num_higher = 0
            for dire in directions:
                if y+dire[1] < 0 or x+dire[0] < 0:
                    num_higher += 1
                    continue
                if debug:
                    print((x, y), (y+dire[1], x+dire[0]))
                try:
                    if debug:
                        print(inp[y,x], inp[y+dire[1],x+dire[0]])
                    if inp[y][x] < inp[y+dire[1],x+dire[0]]:
                        if debug: 
                            print('higher')
                        num_higher += 1
                except IndexError:
                    num_higher += 1
            if num_higher == 4:
                mask[y,x] = True

    risk = ((1 + inp) * mask)

    return risk.sum()

print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=False))
