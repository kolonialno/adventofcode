from __future__ import annotations

import json
from functools import cmp_to_key
from itertools import zip_longest
from pathlib import Path
from typing import TypeAlias

IntOrList: TypeAlias = int | list["IntOrList"]


def solve_a(data: str) -> int:
    pairs = parse(data)
    correctly_ordered = []
    for i, (left, right) in enumerate(pairs):
        if cmp(left, right) == -1:
            correctly_ordered.append(i + 1)
    return sum(correctly_ordered)


def solve_b(data: str) -> int:
    divider_a: IntOrList = [[2]]
    divider_b: IntOrList = [[6]]
    pairs = parse(data)
    pairs.append((divider_a, divider_b))
    packets = sorted([p for pair in pairs for p in pair], key=cmp_to_key(cmp))
    return (packets.index(divider_a) + 1) * (packets.index(divider_b) + 1)


def parse(data: str) -> list[tuple[IntOrList, IntOrList]]:
    result = []
    for line_pair in data.split("\n\n"):
        line1, line2 = line_pair.splitlines()
        result.append((json.loads(line1), json.loads(line2)))
    return result


def cmp(left: IntOrList | None, right: IntOrList | None) -> int:
    # Handle None (a list ran out of items)
    if left is None:
        return -1
    if right is None:
        return 1

    # Handle lists
    if isinstance(left, list) and isinstance(right, int):
        return cmp(left, [right])
    elif isinstance(left, int) and isinstance(right, list):
        return cmp([left], right)
    elif isinstance(left, list) and isinstance(right, list):
        for lft, rgt in zip_longest(left, right):
            if (res := cmp(lft, rgt)) != 0:
                return res
        return 0

    # Handle ints
    assert isinstance(left, int) and isinstance(right, int)
    if left < right:
        return -1
    elif left == right:
        return 0
    else:
        return 1


def test() -> None:
    data = Path("test13.txt").read_text()
    assert solve_a(data) == 13
    assert solve_b(data) == 140


if __name__ == "__main__":
    data = Path("input13.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
