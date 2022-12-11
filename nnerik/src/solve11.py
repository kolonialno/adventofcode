import math
import operator


class Monkey:
    def __init__(self, spec):
        lines = spec.splitlines()

        self.worries = [int(i) for i in lines[1][18:].split(", ")]

        expression = lines[2][19:].split()
        op = {"+": operator.add, "*": operator.mul}[expression[1]]
        arg1 = None if expression[0] == "old" else int(expression[0])
        arg2 = None if expression[2] == "old" else int(expression[2])
        self.operation = lambda old: op(
            arg1 if arg1 is not None else old, arg2 if arg2 is not None else old
        )

        self.divisor = int(lines[3][21:])
        if_divisible = int(lines[4][29:])
        if_not_divisible = int(lines[5][30:])
        self.next_monkey = (
            lambda n: if_not_divisible if n % self.divisor else if_divisible
        )


def get_monkeys(data):
    return [Monkey(spec) for spec in data.strip().split("\n\n")]


def run_monkeys(monkeys, divisor, lcm=None):
    inspections = []
    for monkey in monkeys:
        inspections.append(len(monkey.worries))
        for worry in (
            monkey.operation(old_worry) // divisor for old_worry in monkey.worries
        ):
            monkeys[monkey.next_monkey(worry)].worries.append(
                worry % lcm if lcm else worry
            )
        monkey.worries = []
    return inspections


def solve(data, divisor, rounds):
    monkeys = get_monkeys(data)
    inspections = [0] * len(monkeys)
    lcm = math.lcm(*(m.divisor for m in monkeys))
    for _ in range(rounds):
        for i, count in enumerate(run_monkeys(monkeys, divisor, lcm)):
            inspections[i] += count
    return operator.mul(*sorted(inspections)[-2:])


def solve1(data):
    return solve(data, 3, 20)


def solve2(data):
    return solve(data, 1, 10000)
