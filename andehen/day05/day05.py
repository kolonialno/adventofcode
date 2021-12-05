import numpy as np


with open("input.txt") as f:
    l = [l.strip() for l in f.readlines()]

p = [[[int(y) for y in x.split(",")] for x in l.split(" -> ")] for l in lines]

P1 = np.zeros((N+1, N+1))
P2 = np.zeros((N+1, N+1))

for (x1, y1), (x2, y2) in p:
    l = max(abs(x2-x1) + 1, abs(y2-y1)+1) # The lenght of the line
    for x, y in list(zip(np.linspace(x1, x2, l, dtype=int), np.linspace(y1, y2, l, dtype=int))): # Iterate over the pair of points
        P2[x, y] += 1
        if (x1 == x2) or (y1 == y2): # Only horizontal or vertical lines
            P1[x, y] += 1

print((P1 > 1).sum())
print((P2 > 1).sum())
