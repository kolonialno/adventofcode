from collections import defaultdict
from itertools import chain, count

input = open(0).read().strip().split("\n")

height, width = len(input), len(input[0])
antennas = defaultdict(list)
for y, line in enumerate(input):
    for x, char in enumerate(line):
        if char != ".":
            antennas[char].append((x, y))


def get_antinodes(x, y, dx, dy, generator):
    for n in generator():
        node = (x + n * dx, y + n * dy)
        if 0 <= node[0] < width and 0 <= node[1] < height:
            yield node
        else:
            break


def count_antinodes(generator):
    antinodes = set()
    for locations in antennas.values():
        for index, loc1 in enumerate(locations):
            for loc2 in locations[index + 1 :]:
                dx, dy = loc2[0] - loc1[0], loc2[1] - loc1[1]
                for antinode in chain(
                    get_antinodes(loc1[0], loc1[1], -dx, -dy, generator),
                    get_antinodes(loc2[0], loc2[1], dx, dy, generator),
                ):
                    antinodes.add(antinode)
    return len(antinodes)


print("Part 1:", count_antinodes(lambda: range(1, 2)))
print("Part 2:", count_antinodes(count))
