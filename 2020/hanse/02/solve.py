from sys import stdin
import re

lines = [line.strip() for line in stdin]

regex = re.compile(r"(\d+)-(\d+) (\w+): (\w+)")


def a():
    total = 0
    for line in lines:
        match = regex.search(line)
        if not match:
            continue

        (start, stop, letter, password) = match.groups()
        letter_count = password.count(letter)

        if letter_count >= int(start) and letter_count <= int(stop):
            total += 1

    return total


def b():
    total = 0
    for line in lines:
        match = regex.search(line)
        if not match:
            continue

        (start, stop, letter, password) = match.groups()

        x = int(start) - 1
        y = int(stop) - 1

        result = (password[x] == letter, password[y] == letter)

        if result in ((True, False), (False, True)):
            total += 1

    return total


print(a())
print(b())
