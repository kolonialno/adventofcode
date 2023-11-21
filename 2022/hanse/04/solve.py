import re
from sys import stdin

REGEX = re.compile(r"^(\d+)-(\d+),(\d+)-(\d+)$")


def parse(line: str) -> tuple[set[int], set[int]]:
    match = REGEX.search(line)
    assert match

    a, b, c, d = map(int, match.groups())
    return set(range(a, b + 1)), set(range(c, d + 1))


lines = [parse(line.strip()) for line in stdin]


def a():
    return len([1 for (a, b) in lines if a.issubset(b) or b.issubset(a)])


def b():
    return len([1 for (a, b) in lines if a & b != set()])


print(a())
print(b())
