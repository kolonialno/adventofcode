from __future__ import annotations

import re
from collections import defaultdict
from pathlib import Path
from typing import TypeAlias

StackNumber: TypeAlias = int
Stack: TypeAlias = list[str]
Cargo: TypeAlias = dict[StackNumber, Stack]

Quantity: TypeAlias = int
Step: TypeAlias = tuple[Quantity, StackNumber, StackNumber]
Procedure: TypeAlias = list[Step]


def parse(data: str) -> tuple[Cargo, Procedure]:
    cargo: Cargo = defaultdict(list)
    procedure: Procedure = []

    lines = data.splitlines()

    while lines:
        line = lines.pop(0)

        if line.startswith(" 1"):
            continue
        if not line:
            break  # Stop building cargo area

        for pos in range(len(line)):
            if (pos + 3) % 4 == 0:
                if line[pos] == " ":
                    continue
                stack_number: StackNumber = (pos + 3) // 4
                cargo[stack_number].insert(0, line[pos])

    while lines:
        line = lines.pop(0)

        if match := re.match("move (\d+) from (\d+) to (\d+)", line):
            procedure.append(
                (int(match.group(1)), int(match.group(2)), int(match.group(3)))
            )

    return (cargo, procedure)


def solve_a(data: str) -> str:
    cargo, procedure = parse(data)

    for (quantity, from_stack, to_stack) in procedure:
        for _ in range(quantity):
            cargo[to_stack].append(cargo[from_stack].pop())

    top_items = []
    num_stacks = len(cargo)
    for i in range(1, num_stacks + 1):
        top_items.append(cargo[i][-1])
    return "".join(top_items)


def solve_b(data: str) -> str:
    cargo, procedure = parse(data)

    for (quantity, from_stack, to_stack) in procedure:
        moving = cargo[from_stack][-quantity:]
        del cargo[from_stack][-quantity:]
        cargo[to_stack].extend(moving)

    top_items = []
    num_stacks = len(cargo)
    for i in range(1, num_stacks + 1):
        top_items.append(cargo[i][-1])
    return "".join(top_items)


def test() -> None:
    data = Path("test05.txt").read_text()
    assert parse(data) == (
        {
            1: ["Z", "N"],
            2: ["M", "C", "D"],
            3: ["P"],
        },
        [
            (1, 2, 1),
            (3, 1, 3),
            (2, 2, 1),
            (1, 1, 2),
        ],
    )
    assert solve_a(data) == "CMZ"
    assert solve_b(data) == "MCD"


if __name__ == "__main__":
    data = Path("input05.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
