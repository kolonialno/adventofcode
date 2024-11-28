from sys import stdin
import string

groups = [group.strip().split("\n") for group in stdin.read().split("\n\n")]


def a():
    total = 0
    for group in groups:
        answered = set()  # type: ignore
        for member in group:
            answered = answered | set(list(member))

        total += len(answered)

    return total


def b():
    total = 0
    for group in groups:
        answered = set(string.ascii_lowercase)  # type: ignore
        for member in group:
            answered = answered & set(list(member))

        total += len(answered)

    return total


print(a())
print(b())
