from sys import stdin
import string

lines = [line.strip() for line in stdin]


def priority(x: str) -> int:
    if x.islower():
        return string.ascii_lowercase.index(x) + 1
    return string.ascii_uppercase.index(x) + 27


def a() -> int:
    priorities = []
    for line in lines:
        common = set(line[: len(line) // 2]) & set(line[len(line) // 2 :])
        priorities.append(priority(common.pop()))

    return sum(priorities)


def b() -> int:
    priorities = []
    for i in range(0, len(lines), 3):
        common = set(lines[i]) & set(lines[i + 1]) & set(lines[i + 2])
        priorities.append(priority(common.pop()))

    return sum(priorities)


print(a())
print(b())
