from sys import stdin

lines = [int(line) for line in stdin]


def a():
    return len([a for (a, b) in zip([None] + lines, lines) if a is not None and a < b])


def b():
    sums = [sum(lines[i : i + 3]) for i in range(len(lines) - 2)]
    return len([a for (a, b) in zip([None] + sums, sums) if a is not None and a < b])


print(a())
print(b())
