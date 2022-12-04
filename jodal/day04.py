from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterator


@dataclass
class Assignment:
    start: int
    stop: int

    def overlaps_fully(self, other: Assignment):
        return self.start <= other.start and self.stop >= other.stop

    def overlaps_partially(self, other: Assignment):
        return (other.start <= self.start <= other.stop) or (
            other.start <= self.stop <= other.stop
        )


def parse(data: str) -> Iterator[tuple[Assignment, Assignment]]:
    for line in data.splitlines():
        elfa, elfb = line.split(",")
        yield (
            Assignment(*map(int, elfa.split("-"))),
            Assignment(*map(int, elfb.split("-"))),
        )


def solve_a(data: str) -> int:
    count = 0
    for (one, two) in parse(data):
        if one.overlaps_fully(two) or two.overlaps_fully(one):
            count += 1
    return count


def solve_b(data: str) -> int:
    count = 0
    for (one, two) in parse(data):
        if one.overlaps_partially(two) or two.overlaps_partially(one):
            count += 1
    return count


def test() -> None:
    data = Path("test04.txt").read_text()
    assert solve_a(data) == 2
    assert solve_b(data) == 4


if __name__ == "__main__":
    data = Path("input04.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
