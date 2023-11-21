from pathlib import Path


def parse(data: str) -> list[int]:
    elves = []
    calories = 0

    for line in data.splitlines():
        if line:
            calories += int(line)
        else:
            elves.append(calories)
            calories = 0
    elves.append(calories)

    return elves


def solve_a(data: str) -> int:
    elves = parse(data)
    return max(elves)


def solve_b(data: str) -> int:
    elves = parse(data)
    return sum(sorted(elves)[-3:])


def test() -> None:
    data = Path("test01.txt").read_text()
    assert solve_a(data) == 24000
    assert solve_b(data) == 45000


if __name__ == "__main__":
    data = Path("input01.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
