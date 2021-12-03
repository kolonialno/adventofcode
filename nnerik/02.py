from functools import reduce


def read_input(filename):
    key = {"forward": (1, 0), "down": (0, 1), "up": (0, -1)}
    with open(filename) as f:
        lines = (line.split(" ") for line in f.readlines())
        return [(key[a], int(b)) for a, b in lines]


def part1(input):
    steps = ((x * d, z * d) for (x, z), d in input)
    x, z = reduce(lambda p1, p2: (p1[0] + p2[0], p1[1] + p2[1]), steps)
    return x * z


def part2(input):
    aim = 0
    x, z = 0, 0
    for (dx, dz), d in input:
        x += dx * d
        z += dx * d * aim
        aim += dz * d
    return x * z


if __name__ == "__main__":
    input = read_input(__file__[:-3] + ".txt")
    print(f"Part 1: {part1(input)}")
    print(f"Part 2: {part2(input)}")
