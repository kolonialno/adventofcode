import numpy as np

with open("input.txt") as f:
    bn = [int(x) for x in f.readline().strip().split(",")]
    f.readline()
    B_raw = f.read().replace("  ", " ")

B = np.array([
    [[int(x) for x in n.strip().split(" ")] for n in b.split("\n")]
    for b in B_raw.split("\n\n")
])

r = B.shape[2]
w = 0

for n in bn:
    B[B == n] = -1
    for a in [1, 2]:
        if np.any(B.sum(axis=a) == -r):
            ix = np.any(B.sum(axis=a) == -r, axis=1)
            s = n * B[ix][B[ix] > 0].sum()
            if w == 0:
                print(s)
            B[ix] = -100
            w += 1
print(s)
