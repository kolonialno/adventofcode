from __future__ import annotations

from pathlib import Path
from typing import TypeAlias

Pos: TypeAlias = tuple[int, int]


def solve_a(data: str) -> int:
    return solve_n(data, rope_length=2)


def solve_b(data: str) -> int:
    return solve_n(data, rope_length=10)


def solve_n(data: str, *, rope_length: int) -> int:
    motions = parse(data)

    rope: list[Pos] = [(0, 0) for _ in range(rope_length)]
    tail_positions: set[Pos] = set([rope[-1]])

    for (direction, steps) in motions:
        for _ in range(steps):
            rope[0] = move_direction(rope[0], direction)

            for i in range(1, rope_length):
                if not is_adjacent(rope[i], rope[i - 1]):
                    rope[i] = move_towards(rope[i], rope[i - 1])

            tail_positions.add(rope[-1])

    return len(tail_positions)


def parse(data: str) -> list[tuple[str, int]]:
    motions = []
    for line in data.splitlines():
        direction, steps = line.split()
        motions.append((direction, int(steps)))
    return motions


def is_adjacent(a: Pos, b: Pos) -> bool:
    return (-1 <= a[0] - b[0] <= 1) and (-1 <= a[1] - b[1] <= 1)


def move_direction(pos: Pos, direction: str) -> Pos:
    match direction:
        case "R":
            return (pos[0] + 1, pos[1])
        case "L":
            return (pos[0] - 1, pos[1])
        case "U":
            return (pos[0], pos[1] + 1)
        case "D":
            return (pos[0], pos[1] - 1)
        case _:
            raise Exception("Unknown direction")


def move_towards(pos: Pos, target: Pos) -> Pos:
    return (
        pos[0] + cmp(target[0], pos[0]),
        pos[1] + cmp(target[1], pos[1]),
    )


def cmp(a: int, b: int) -> int:
    # Just like in good old Python 2
    if a < b:
        return -1
    elif a == b:
        return 0
    else:
        return 1


def test() -> None:
    data = Path("test09.txt").read_text()
    assert solve_a(data) == 13
    assert solve_b(data) == 1
    data = Path("test09b.txt").read_text()
    assert solve_b(data) == 36


if __name__ == "__main__":
    data = Path("input09.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
