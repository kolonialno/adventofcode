import re
from collections import Counter
from sys import stdin

REGEX = re.compile(r"^(\d+),(\d+) -> (\d+),(\d+)$")


def parse(line):
    match = REGEX.search(line)
    if not match:
        raise ValueError()
    return [int(x) for x in match.groups()]


lines = [parse(line.strip()) for line in stdin]


def solve(use_diagonals):
    result = Counter()
    for line in lines:
        x1, y1, x2, y2 = line

        if x1 != x2 and y1 != y2:
            if not use_diagonals:
                continue

            x, y = x1, y1
            result[(x, y)] += 1

            while (x, y) != (x2, y2):
                x += 1 if x2 > x1 else -1
                y += 1 if y2 > y1 else -1
                result[(x, y)] += 1

        else:
            ys = range(y1, y2 + 1) if y1 <= y2 else range(y1, y2 - 1, -1)
            xs = range(x1, x2 + 1) if x1 <= x2 else range(x1, x2 - 1, -1)

            for y in ys:
                for x in xs:
                    result[(x, y)] += 1

    return len([x for x in result.values() if x >= 2])


def a():
    return solve(False)


def b():
    return solve(True)


print(a())
print(b())
