import numpy as np

with open("input.txt") as f:
    P = np.array([int(x) for x in f.readline().split(",")])

# Part 1
print(abs(P - np.median(P)).sum())

# Part 2
def int_sum(n):
    return n * (n + 1) / 2

F = np.inf
for p in range(min(P), max(P) + 1):
    f = sum([int_sum(d) for d in abs(P - p)])
    if f < F:
        F = f

print(F)
