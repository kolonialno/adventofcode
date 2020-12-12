from sys import stdin
import math


def parse(line):
    return (line[0], int(line[1:]))


directions = [parse(line.strip()) for line in stdin]


def a():
    x = 0
    y = 0

    rotations = ["E", "S", "W", "N"]
    current_rotation = 0

    def _move(instruction, value):
        nonlocal x, y
        if instruction == "N":
            y += value
        elif instruction == "E":
            x += value
        elif instruction == "S":
            y -= value
        elif instruction == "W":
            x -= value

    for (instruction, value) in directions:
        if instruction == "R":
            current_rotation = (current_rotation + value // 90) % len(rotations)
        elif instruction == "L":
            current_rotation = (current_rotation - value // 90) % len(rotations)
        elif instruction == "F":
            _move(rotations[current_rotation], value)
        else:
            _move(instruction, value)

    return abs(x) + abs(y)


def b():
    x = 0
    y = 0

    waypoint_x = 10
    waypoint_y = 1

    def _rotate(x0, y0, degrees):
        rad = degrees * math.pi / 180
        return (
            int(x0 * math.cos(rad)) - int(y0 * math.sin(rad)),
            int(x0 * math.sin(rad)) + int(y0 * math.cos(rad)),
        )

    for (instruction, value) in directions:
        if instruction == "R":
            waypoint_x, waypoint_y = _rotate(waypoint_x, waypoint_y, -value)
        elif instruction == "L":
            waypoint_x, waypoint_y = _rotate(waypoint_x, waypoint_y, value)
        elif instruction == "F":
            x += waypoint_x * value
            y += waypoint_y * value
        elif instruction == "N":
            waypoint_y += value
        elif instruction == "E":
            waypoint_x += value
        elif instruction == "S":
            waypoint_y -= value
        elif instruction == "W":
            waypoint_x -= value

    return abs(x) + abs(y)


print(a())
print(b())
