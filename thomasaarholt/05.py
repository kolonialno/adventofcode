import numpy as np
import re
from itertools import product


test = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2""".split("\n")
test1_result = 5
test2_result = 12

with open("inputs/05.txt") as f:
    txt = f.read().strip().split("\n")

def get_integer_points(list_of_start_stop):
    "Found on stackoverflow"
    d = np.diff(list_of_start_stop, axis=0)[0]
    j = np.argmax(np.abs(d))
    D = d[j]
    aD = np.abs(D)
    return list_of_start_stop[0] + (np.outer(np.arange(aD + 1), d) + (aD>>1)) // aD

def read(txt):
    coords = []
    for s in txt:
        pattern = "(\d+),(\d+) -> (\d+),(\d+)"
        m = re.match(pattern, s)
        coords.append(list(map(int,m.groups())))
    return coords

def get_result(coords):
    grid = np.zeros((1000,1000))
    for x1, y1, x2, y2 in coords:
        points = get_integer_points(((x1, y1), (x2, y2)))
        for x, y in points:
            grid[y, x] += 1
    result = (grid > 1).sum()
    return result

def func1(txt):
    coords = np.array(read(txt))
    x1, y1, x2, y2 = coords.T
    mask = (x1 == x2) | (y1 == y2) # get only straight lines
    coords_straight = coords[mask]
    return get_result(coords_straight)

def func2(txt):
    coords = read(txt)
    return get_result(coords)

def test1():
    result = func1(test)
    assert result == test1_result
    print("TEST 1 PASS")

def test2():
    result = func2(test)
    assert result == test2_result
    print("TEST 2 PASS")

test1()
print(func1(txt))
test2()
print(func2(txt))