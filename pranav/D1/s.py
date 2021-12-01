from itertools import pairwise

def triplewise(iterable):
    "Return overlapping triplets from an iterable"
    # triplewise('ABCDEFG') -> ABC BCD CDE DEF EFG
    for (a, _), (b, c) in pairwise(pairwise(iterable)):
        yield a, b, c    

with open("input.txt") as f:
    lines = f.readlines()

    min_value = 0
    inc = 0
    for l in lines:
        new_value = int(l.strip())
        if min_value and (new_value > min_value):
            inc += 1
        min_value = new_value

    print(inc)

with open("input.txt") as f:
    lines = f.readlines()
    min_value = 0
    inc = 0
    for x, y, z in triplewise(lines):
        new_value = int(x.strip()) + int(y.strip()) + int(z.strip())
        if min_value and (new_value > min_value):
            inc += 1
        min_value = new_value
        
    print(inc)