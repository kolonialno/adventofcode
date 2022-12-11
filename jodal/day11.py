from __future__ import annotations

from dataclasses import dataclass
from math import lcm
from operator import add, mul
from pathlib import Path
from typing import Callable


def solve_a(data: str) -> int:
    return solve_n(data, num_rounds=20, worry_divisor=3)


def solve_b(data: str) -> int:
    return solve_n(data, num_rounds=10000, worry_divisor=None)


def solve_n(data: str, *, num_rounds: int, worry_divisor: int | None) -> int:
    monkeys = Monkey.from_data(data)
    worry_lcm = lcm(*[m.divisor for m in monkeys])

    for _round in range(num_rounds):
        for monkey in monkeys:
            while monkey.items:
                item = monkey.items.pop(0)

                # Monkey inspects item, worry increases
                if monkey.operand is None:
                    item = monkey.operator(item, item)
                else:
                    item = monkey.operator(item, monkey.operand)

                # Monkey gets bored, worry decreases
                if worry_divisor:
                    item //= worry_divisor
                else:
                    item %= worry_lcm

                # Monkey decides where to throw item
                if item % monkey.divisor == 0:
                    monkeys[monkey.true_target].items.append(item)
                else:
                    monkeys[monkey.false_target].items.append(item)

                monkey.inspections += 1

    return mul(*sorted(m.inspections for m in monkeys)[-2:])


@dataclass
class Monkey:
    items: list[int]
    operator: Callable[[int, int], int]
    operand: int | None
    divisor: int
    true_target: int
    false_target: int
    inspections: int = 0

    @classmethod
    def from_data(cls, data: str) -> list[Monkey]:
        return [cls.from_block(block) for block in data.strip().split("\n\n")]

    @classmethod
    def from_block(cls, data: str) -> Monkey:
        items: list[int]
        operator: Callable[[int, int], int]
        operand: int | None
        divisor: int
        true_target: int
        false_target: int

        for line in data.splitlines():
            match line.split():
                case ["Starting", "items:", *args]:
                    items = [int(arg.replace(",", "")) for arg in args]
                case ["Operation:", "new", "=", "old", "+", val]:
                    operator = add
                    operand = int(val) if val != "old" else None
                case ["Operation:", "new", "=", "old", "*", val]:
                    operator = mul
                    operand = int(val) if val != "old" else None
                case ["Test:", "divisible", "by", num]:
                    divisor = int(num)
                case ["If", "true:", "throw", "to", "monkey", num]:
                    true_target = int(num)
                case ["If", "false:", "throw", "to", "monkey", num]:
                    false_target = int(num)
                case _:
                    pass

        return Monkey(
            items=items,
            operator=operator,
            operand=operand,
            divisor=divisor,
            true_target=true_target,
            false_target=false_target,
        )


def test() -> None:
    data = Path("test11.txt").read_text()
    assert solve_a(data) == 10_605
    assert solve_b(data) == 2_713_310_158


if __name__ == "__main__":
    data = Path("input11.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
