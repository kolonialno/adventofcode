from sys import stdin

numbers = [int(line) for line in stdin]


def a():
    for i in numbers:
        for j in numbers:
            if i + j == 2020:
                return i * j


def b():
    for i in numbers:
        for j in numbers:
            for k in numbers:
                if i + j + k == 2020:
                    return i * j * k


print(a())
print(b())
