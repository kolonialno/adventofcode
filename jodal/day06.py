from __future__ import annotations

from pathlib import Path
from typing import Iterator, TypeVar

T = TypeVar("T")


def sliding_window(lst: list[T], window_size: int) -> Iterator[tuple[int, list[T]]]:
    if len(lst) <= window_size:
        return (len(lst), lst)
    for i in range(len(lst)):
        consumed = i + window_size
        yield (consumed, lst[i:consumed])


def solve_a(data: str) -> int:
    for (consumed, window) in sliding_window(list(data), 4):
        if len(set(window)) == 4:
            return consumed
    raise ValueError("Start of packet not found")


def solve_b(data: str) -> int:
    for (consumed, window) in sliding_window(list(data), 14):
        if len(set(window)) == 14:
            return consumed
    raise ValueError("Start of message not found")


def test() -> None:
    data = Path("test06.txt").read_text()
    assert solve_a(data) == 7
    assert solve_b(data) == 19


if __name__ == "__main__":
    data = Path("input06.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
