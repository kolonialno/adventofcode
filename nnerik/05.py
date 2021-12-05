from collections import defaultdict
import re


def read_input(filename):
    with open(filename) as f:
        return [tuple(map(int, (re.findall("\d+", line)))) for line in f]


def count_overlaps(lines):
    seafloor = defaultdict(int)
    for x1, y1, x2, y2 in lines:
        dx = 1 if x1 < x2 else -1 if x1 > x2 else 0
        dy = 1 if y1 < y2 else -1 if y1 > y2 else 0
        for i in range(max(abs(x1 - x2), abs(y1 - y2)) + 1):
            seafloor[(x1 + i * dx, y1 + i * dy)] += 1

    return sum(overlaps > 1 for overlaps in seafloor.values())


if __name__ == "__main__":
    input = read_input(__file__[:-3] + ".txt")

    # Part 1
    result = count_overlaps(filter(lambda l: l[0] == l[2] or l[1] == l[3], input))
    print(f"Part 1: {result}")

    # Part 2
    result = count_overlaps(input)
    print(f"Part 2: {result}")
