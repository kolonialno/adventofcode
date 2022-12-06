from sys import stdin

stream = next(stdin)


def solve(k):
    for i in range(k, len(stream)):
        xs = [stream[i - j] for j in range(k)]
        if len(xs) == len(set(xs)):
            return i + 1


def a():
    return solve(4)


def b():
    return solve(14)


print(a())
print(b())
