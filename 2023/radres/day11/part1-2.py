from helpers import *
import os
import time
with open("input.txt", "r") as f:
    out = f.read()

#out = """...#......
#.......#..
##.........
#..........
#......#...
#.#........
#.........#
#..........
#.......#..
##...#....."""

isPart2 = True

def find_all_pairs(points):
    pairs = []
    for i in range(len(points)):
        for j in range(i + 1, len(points)):
            pairs.append((points[i], points[j]))
    return pairs

def find_points(grid):
    points = []
    for r,line in enumerate(grid):
        for c,el in enumerate(line):
            if el == "#":
                points.append((r,c))
    return points

def find_distance(point1, point2, expanded_rows, expanded_cols):
    dist = abs(point1[0]-point2[0]) + abs(point2[1]-point1[1])

    minr, maxr = min(point1[0], point2[0]), max(point1[0], point2[0])
    minc, maxc = min(point1[1], point2[1]), max(point1[1], point2[1])
    for r in range(minr, maxr):
        if r in expanded_rows:
            dist += 99999 if isPart2 else 1
    for c in range(minc,maxc):
        if c in expanded_cols:
            dist += 99999 if isPart2 else 1
    return dist

grid = []
for line in out.splitlines():
    vec = []
    for c in line:
        vec.append(c)
    grid.append(vec)

points = find_points(grid)


expanded_rows = []
expanded_cols= []

for r,line in enumerate(grid):
    expanded = True
    for c,el in enumerate(line):
        if el == "#":
            expanded= False
    if expanded:
        expanded_rows.append(r)

for c in range(len(grid[0])):
    expanded = True
    for r,_ in enumerate(grid):
        if grid[r][c] == "#":
            expanded = False
    if expanded:
        expanded_cols.append(c)

pairs = find_all_pairs(points)

result = 0
for pair in pairs:
    point1, point2 = pair
    result += find_distance(point1, point2, expanded_rows, expanded_cols)

print(result)

