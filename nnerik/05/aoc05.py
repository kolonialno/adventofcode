from collections import defaultdict
import re


def count_overlaps(lines):
    seafloor = defaultdict(int)
    for x1, y1, x2, y2 in lines:
        dx = 1 if x1 < x2 else -1 if x1 > x2 else 0
        dy = 1 if y1 < y2 else -1 if y1 > y2 else 0
        for i in range(max(x1 - x2, x2 - x1, y1 - y2, y2 - y1) + 1):
            seafloor[(x1 + i * dx, y1 + i * dy)] += 1

    return sum(1 for n in seafloor.values() if n > 1)


with open("input.txt") as f:
    input = [tuple(int(n) for n in (re.findall("\d+", line))) for line in f]

print("Part 1:", count_overlaps(l for l in input if l[0] == l[2] or l[1] == l[3]))
print("Part 2:", count_overlaps(input))
