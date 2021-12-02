from functools import reduce


def one(filename):
    (pos, depth) = reduce(engine_v1, commands(filename), (0, 0))
    return pos * depth


def two(filename):
    (pos, depth, _aim) = reduce(engine_v2, commands(filename), (0, 0, 0))
    return pos * depth


def commands(filename):
    return ((op, int(val)) for (op, val) in (line.split() for line in open(filename)))


def engine_v1(state, command):
    (pos, depth) = state
    (op, val) = command
    if op == "forward":
        pos += val
    elif op == "down":
        depth += val
    elif op == "up":
        depth -= val
    return (pos, depth)


def engine_v2(state, command):
    (pos, depth, aim) = state
    (op, val) = command
    if op == "forward":
        pos += val
        depth += aim * val
    elif op == "down":
        aim += val
    elif op == "up":
        aim -= val
    return (pos, depth, aim)


def test_one():
    assert one("test02.txt") == 150


def test_two():
    assert two("test02.txt") == 900


if __name__ == "__main__":
    print(one("input02.txt"))
    print(two("input02.txt"))
