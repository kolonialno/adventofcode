from __future__ import annotations

from pathlib import Path


def get_rucksacks(data: str) -> list[tuple[set, set]]:
    result = []
    for line in data.splitlines():
        compartment_size = len(line) // 2
        result.append((set(line[:compartment_size]), set(line[compartment_size:])))
    return result


def partition(lst, chunk_size):
    for i in range(0, len(lst), chunk_size):
        yield lst[i : i + chunk_size]


def priority(item: str) -> int:
    if "a" <= item <= "z":
        return ord(item) - ord("a") + 1
    elif "A" <= item <= "Z":
        return ord(item) - ord("A") + 27
    return 0


def solve_a(data: str) -> int:
    sum = 0
    for first, second in get_rucksacks(data):
        duplicates = first.intersection(second)
        for item in duplicates:
            sum += priority(item)
    return sum


def solve_b(data: str) -> int:
    sum = 0
    for group in partition(get_rucksacks(data), 3):
        badges = (
            (group[0][0].union(group[0][1]))
            .intersection((group[1][0].union(group[1][1])))
            .intersection((group[2][0].union(group[2][1])))
        )
        assert len(badges) == 1
        sum += priority(badges.pop())
    return sum


def test() -> None:
    data = Path("test03.txt").read_text()
    assert solve_a(data) == 157
    assert solve_b(data) == 70


if __name__ == "__main__":
    data = Path("input03.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
