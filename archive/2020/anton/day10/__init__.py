from collections import Counter, defaultdict
from math import prod


def load_input(filename):
    with open(filename) as in_file:
        jolts = sorted(int(x) for x in in_file)
    x = [0]
    x.extend(jolts)
    x.append(max(jolts) + 3)
    return x


def count_steps(jolts):
    counter = Counter()
    while x := jolts.pop():
        counter[x - jolts[-1]] += 1
    return counter


def count_combinations(jolts):
    pos = defaultdict(int)
    pos[jolts[-1]] = 1
    for i in jolts[-2::-1]:
        pos[i] = pos[i + 1] + pos[i + 2] + pos[i + 3]
    return pos[0]


def testcase() -> int:
    return prod(count_steps(load_input("inputs/10testcase.txt")).values())


def testcase2() -> int:
    return prod(count_steps(load_input("inputs/10testcase2.txt")).values())


def main() -> int:
    return prod(count_steps(load_input("inputs/10.txt")).values())


def testcase_secondary() -> int:
    return count_combinations(load_input("inputs/10testcase.txt"))


def testcase_secondary2() -> int:
    return count_combinations(load_input("inputs/10testcase2.txt"))


def secondary() -> int:
    return count_combinations(load_input("inputs/10.txt"))
