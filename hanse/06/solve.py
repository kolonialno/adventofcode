from collections import deque
from sys import stdin

fishes = [int(x) for x in next(stdin).strip().split(",")]


def solve(days):
    counter = deque([0 for _ in range(9)])
    for fish in fishes:
        counter[fish] += 1

    for _ in range(days):
        new_fishes = counter[0]
        counter.rotate(-1)
        counter[6] += new_fishes

    return sum(counter)


def a():
    return solve(80)


def b():
    return solve(256)


print(a())
print(b())
