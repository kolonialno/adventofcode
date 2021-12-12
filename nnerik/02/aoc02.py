from functools import reduce


def nav1(input):
    steps = ((x * d, z * d) for (x, z), d in input)
    x, z = reduce(lambda p1, p2: (p1[0] + p2[0], p1[1] + p2[1]), steps)
    return x * z


def nav2(input):
    aim, x, z = 0, 0, 0
    for (dx, dz), d in input:
        x += dx * d
        z += dx * d * aim
        aim += dz * d
    return x * z


with open("input.txt") as f:
    course = [
        ({"forward": (1, 0), "down": (0, 1), "up": (0, -1)}[cmd], int(val))
        for cmd, val in (line.split() for line in f)
    ]

print("Part 1:", nav1(course))
print("Part 2:", nav2(course))
