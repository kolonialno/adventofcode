import operator
import sympy


ops = {"+": operator.add, "/": operator.truediv, "-": operator.sub, "*": operator.mul}


def eval_monkey(monkeys: dict, monkey: str) -> int:
    match monkeys[monkey].split():
        case [value]:
            return int(value)
        case [monkey_1, op, monkey_2]:
            return ops[op](
                eval_monkey(monkeys, monkey_1), eval_monkey(monkeys, monkey_2)
            )
        case _:
            raise Exception("wtf %s", monkey)


def get_equation(monkeys: dict, monkey: str) -> str:

    if monkey == "humn":
        return "x"

    match monkeys[monkey].split():
        case [value]:
            return value
        case [monkey_1, op, monkey_2]:
            return f"({get_equation(monkeys, monkey_1)} {op} {get_equation(monkeys, monkey_2)})"
        case _:
            raise Exception("wtf %s", monkey)


monkeys = {}
with open("input.txt") as f:
    for l in f.readlines():
        id, job = [s.strip() for s in l.split(":")]
        monkeys[id] = job

print(eval_monkey(monkeys, "root"))

# Part two
rhs, _, lhs = monkeys["root"].split()
print(sympy.simplify(get_equation(monkeys, rhs)))
print(sympy.simplify(get_equation(monkeys, lhs)))

# Solved it manually :)
