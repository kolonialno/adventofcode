from sys import stdin
from itertools import combinations

numbers = [int(line) for line in stdin]
preamble = 25


def a():
    for index, number in enumerate(numbers[preamble:], start=preamble):
        found = False
        for (x, y) in combinations(numbers[index - preamble : index], 2):
            if number == x + y:
                found = True
                break

        if not found:
            return number

    return "not-solved"


def b():
    needle = a()
    for i in range(len(numbers)):
        accumulator = 0
        xs = []
        for j in range(i, len(numbers)):
            accumulator += numbers[j]
            xs.append(numbers[j])

            if accumulator > needle:
                break

            if accumulator == needle:
                xs.sort()
                return xs[0] + xs[-1]

    return "not-solved"


print(a())
print(b())
