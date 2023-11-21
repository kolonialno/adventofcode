def score(their, our):
    return ((our - their + 1) % 3 - 1) * 3 + 4 + our


def solve1(data):
    total = 0
    for line in data.strip().split("\n"):
        first, last = line.split()
        total += score("ABC".index(first), "XYZ".index(last))
    return total


def solve2(data):
    total = 0
    for line in data.strip().split("\n"):
        first, last = line.split()
        their = "ABC".index(first)
        our = (their + "XYZ".index(last) - 1) % 3
        total += score(their, our)
    return total
