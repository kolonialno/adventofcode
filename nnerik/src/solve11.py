import math
import operator


class Monkey:
    def __init__(self, spec):
        lines = spec.splitlines()

        self.worries = [int(i) for i in lines[1][18:].split(", ")]

        arg1, op, arg2 = lines[2][19:].split()
        self.operator = {"+": operator.add, "*": operator.mul}[op]
        self.arg1 = int(arg1) if arg1.isdigit() else arg1
        self.arg2 = int(arg2) if arg2.isdigit() else arg2

        self.divisor = int(lines[3][21:])
        self.if_true = int(lines[4][29:])
        self.if_false = int(lines[5][30:])

    def operation(self, old):
        return self.operator(
            old if self.arg1 == "old" else self.arg1,
            old if self.arg2 == "old" else self.arg2,
        )

    def next_monkey(self, worry):
        return self.if_false if worry % self.divisor else self.if_true


def get_monkeys(data):
    return [Monkey(spec) for spec in data.strip().split("\n\n")]


def run_monkeys(monkeys, divisor, lcm):
    inspections = []
    for monkey in monkeys:
        inspections.append(len(monkey.worries))
        for worry in (
            monkey.operation(old_worry) // divisor for old_worry in monkey.worries
        ):
            monkeys[monkey.next_monkey(worry)].worries.append(worry % lcm)
        monkey.worries = []
    return inspections


def solve(data, divisor, rounds):
    monkeys = get_monkeys(data)
    inspections = [0] * len(monkeys)
    lcm = math.lcm(*(m.divisor for m in monkeys))
    for _ in range(rounds):
        inspections = map(operator.add, run_monkeys(monkeys, divisor, lcm), inspections)
    return operator.mul(*sorted(inspections)[-2:])


def solve1(data):
    return solve(data, 3, 20)


def solve2(data):
    return solve(data, 1, 10000)
