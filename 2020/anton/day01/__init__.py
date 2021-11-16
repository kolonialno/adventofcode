import itertools
from math import prod


def parse_entries() -> list:
    with open("inputs/01.txt") as in_file:
        rows = in_file.readlines()
    return {int(row) for row in rows}


def factor_sum(sought_sum, number_set, factors=2) -> int:
    for x in number_set:
        if factors > 2:
            y = factor_sum(sought_sum - x, number_set, factors - 1)
            if y:
                return x * y
        elif sought_sum - x in number_set:
            return x * (sought_sum - x)


def main() -> int:
    entries = parse_entries()
    return factor_sum(2020, entries)


def secondary() -> int:
    entries = parse_entries()
    return factor_sum(2020, entries, 3)


def itertools_factor_sum(entries, factors):
    combinations = itertools.combinations(entries, factors)
    for items in combinations:
        if sum(items) == 2020:
            return prod(items)


def main_itertools() -> int:
    entries = parse_entries()
    return itertools_factor_sum(entries, 2)


def secondary_itertools() -> int:
    entries = parse_entries()
    return itertools_factor_sum(entries, 3)
