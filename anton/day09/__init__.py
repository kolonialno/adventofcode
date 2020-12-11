from itertools import combinations
from collections import deque


def load_input(filename):
    with open(filename) as in_file:
        return [int(x) for x in in_file]


def has_sum_factor(entries, factors, result):
    combs = combinations(entries, factors)
    for items in combs:
        if sum(items) == result:
            return True


def find_off_number(sequence, preamble_len):
    window = deque(sequence[:preamble_len], preamble_len)
    remainder = sequence[preamble_len:]
    for x in remainder:
        if not has_sum_factor(window, 2, x):
            return x
        window.append(x)


def sum_to_off_number(sequence, num):
    running = deque()
    for x in sequence[: sequence.index(num)]:
        while sum(running) > num:
            running.popleft()
        if sum(running) == num:
            return min(running) + max(running)
        running.append(x)


def testcase() -> int:
    return find_off_number(load_input("inputs/09testcase.txt"), 5)


def main() -> int:
    return find_off_number(load_input("inputs/09.txt"), 25)


def testcase_secondary() -> int:
    sequence = load_input("inputs/09testcase.txt")
    num = find_off_number(sequence, 5)
    return sum_to_off_number(sequence, num)


def secondary() -> int:
    sequence = load_input("inputs/09.txt")
    num = find_off_number(sequence, 25)
    return sum_to_off_number(sequence, num)
