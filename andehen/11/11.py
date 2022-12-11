from copy import deepcopy
import numpy as np

raw_monkeys = []

with open("input.txt") as f:
    for monkey_text in f.read().split("\n\n"):
        raw_props = monkey_text.split("\n")

        id = int(raw_props[0].split()[1][:-1])
        items = [int(wl) for wl in raw_props[1].strip().split(":")[1].split(",")]
        operation = raw_props[2].strip().split(":")[1].strip().split("=")[1].strip()
        test = int(raw_props[3].strip().split(":")[1].split()[2])
        if_true = int(raw_props[4].strip().split(":")[1].split()[3])
        if_false = int(raw_props[5].strip().split(":")[1].split()[3])

        raw_monkeys.append((items, operation, test, if_true, if_false))


tests = np.array([monkey[2] for monkey in raw_monkeys])


def get_monkey_bussiness(n, divide_by):
    monkeys = []
    if divide_by != 1:
        monkeys = deepcopy(raw_monkeys)
    else:
        monkeys = []
        for monkey in raw_monkeys:
            items = monkey[0]
            item_modulus = []
            for i in items:
                item_modulus.append(i % tests)

            monkeys.append((item_modulus,) + monkey[1:])

    inspections = [0] * len(monkeys)
    for _ in range(n):
        for i, monkey in enumerate(monkeys):
            while len(monkey[0]) > 0:
                inspections[i] += 1
                old = monkey[0].pop(0)
                new = eval(monkey[1])
                if divide_by == 1:
                    new %= tests
                    test_true = new[i] == 0
                else:
                    new //= divide_by
                    test_true = new % monkey[2] == 0
                if test_true:
                    monkeys[monkey[3]][0].append(new)
                else:
                    monkeys[monkey[4]][0].append(new)
    return np.prod(sorted(inspections, reverse=True)[:2])


print(get_monkey_bussiness(20, 3))
print(get_monkey_bussiness(10000, 1))
