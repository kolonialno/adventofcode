from __future__ import annotations

from pathlib import Path


def run_program(data: str) -> list[int]:
    x = 1
    cycles = []

    for line in data.splitlines():
        match line.split():
            case ["noop"]:
                cycles.append(x)
            case ["addx", value]:
                cycles.append(x)
                cycles.append(x)
                x += int(value)

    return cycles


def solve_a(data: str) -> int:
    cycles = run_program(data)
    return sum((i + 1) * cycles[i] for i in range(19, len(cycles), 40))


def solve_b(data: str) -> str:
    NUM_ROWS = 6
    NUM_COLS = 40
    cycles = run_program(data)
    pixels = []

    for i in range(NUM_ROWS * NUM_COLS):
        pos = i % NUM_COLS
        x = cycles[i]

        if x - 1 <= pos <= x + 1:
            pixels.append("#")
        else:
            pixels.append(".")

        if pos + 1 == NUM_COLS:
            pixels.append("\n")

    return "".join(pixels).strip()


def test() -> None:
    data = Path("test10.txt").read_text()
    assert solve_a(data) == 13140
    assert (
        solve_b(data)
        == """##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."""
    )


if __name__ == "__main__":
    data = Path("input10.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
