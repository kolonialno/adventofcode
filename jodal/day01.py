from itertools import pairwise, starmap
from operator import lt

from aoclib import triplewise


def one(a):
    return sum(starmap(lt, pairwise(map(int, open(a)))))


def test_one():
    assert one("test01.txt") == 7


def two(a):
    return sum(starmap(lt, pairwise(map(sum, triplewise(map(int, open(a)))))))


def test_two():
    assert two("test01.txt") == 5


if __name__ == "__main__":
    print(one("input01.txt"))
    print(two("input01.txt"))
