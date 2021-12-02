from sys import stdin


def parse(line):
    direction, units = line.split()
    return direction, int(units)


lines = [parse(line.strip()) for line in stdin]


def a():
    x, y = 0, 0
    for (direction, units) in lines:
        if direction == "forward":
            x += units
        elif direction == "down":
            y += units
        elif direction == "up":
            y -= units

    return x * y


def b():
    x, y, aim = 0, 0, 0
    for (direction, units) in lines:
        if direction == "forward":
            x += units
            y += aim * units
        elif direction == "down":
            aim += units
        elif direction == "up":
            aim -= units

    return x * y


print(a())
print(b())
