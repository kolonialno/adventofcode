from itertools import pairwise


def triplewise(it):
    for (a, _), (b, c) in pairwise(pairwise(it)):
        yield a, b, c
