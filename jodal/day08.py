from __future__ import annotations

from pathlib import Path
from typing import Iterable


def parse(data: str) -> list[list[int]]:
    return [[int(char) for char in line] for line in data.splitlines()]


def visible_from_outside(height: int, trees: Iterable[int]) -> bool:
    return all(val < height for val in trees)


def viewing_distance(height: int, trees: Iterable[int]) -> int:
    distance = 0
    for i, val in enumerate(trees, start=1):
        distance = i
        if val >= height:
            break
    return distance


def solve_a(data: str) -> int:
    forest = parse(data)

    visible = [[False for _ in row] for row in forest]
    for y, row in enumerate(forest):
        for x, height in enumerate(row):
            trees_left = reversed(forest[y][:x])
            trees_right = forest[y][x + 1 :]
            trees_up = reversed([forest[i][x] for i in range(y)])
            trees_down = [forest[i][x] for i in range(y + 1, len(forest))]
            visible[y][x] = (
                visible_from_outside(height, trees_left)
                or visible_from_outside(height, trees_right)
                or visible_from_outside(height, trees_up)
                or visible_from_outside(height, trees_down)
            )

    return sum(sum(row) for row in visible)


def solve_b(data: str) -> int:
    forest = parse(data)

    score = [[0 for _ in row] for row in forest]
    for y, row in enumerate(forest):
        for x, height in enumerate(row):
            trees_left = reversed(forest[y][:x])
            trees_right = forest[y][x + 1 :]
            trees_up = reversed([forest[i][x] for i in range(y)])
            trees_down = [forest[i][x] for i in range(y + 1, len(forest))]
            score[y][x] = (
                viewing_distance(height, trees_left)
                * viewing_distance(height, trees_right)
                * viewing_distance(height, trees_up)
                * viewing_distance(height, trees_down)
            )

    return max(max(row) for row in score)


def test() -> None:
    data = Path("test08.txt").read_text()
    assert solve_a(data) == 21
    assert solve_b(data) == 8


if __name__ == "__main__":
    data = Path("input08.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
