from collections import Counter
from functools import reduce
from itertools import zip_longest


def one(filename):
    lines = diagnostics(filename)
    columns = zip_longest(*lines)
    digit_counts = map(Counter, columns)
    gamma_bits = [(0 if c[0] > c[1] else 1) for c in digit_counts]
    gamma = bits2int(gamma_bits)
    epsilon = gamma ^ (2 ** len(gamma_bits) - 1)
    power_consumption = gamma * epsilon
    return power_consumption


def two(filename):
    pass


def diagnostics(filename):
    return (tuple(map(int, list(line.strip()))) for line in open(filename))


def bits2int(bits):
    n = len(bits)
    return sum(bit << (n - i - 1) for i, bit in enumerate(bits))


def test_one():
    assert one("test03.txt") == 198


def xtest_two():
    assert two("test03.txt") == 0


if __name__ == "__main__":
    print(one("input03.txt"))
    print(two("input03.txt"))
