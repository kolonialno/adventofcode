from collections import Counter
from itertools import starmap


def parse_answers_file(filename) -> list:
    with open(filename) as in_file:
        data = in_file.read()
    answer_data = (Counter(x.replace("\n", "")) for x in data.split("\n\n"))
    return answer_data


def parse_answers_with_totals(filename) -> list:
    with open(filename) as in_file:
        data = in_file.read()
    answer_data = (
        (x.strip().count("\n") + 1, Counter(x.replace("\n", "")))
        for x in data.split("\n\n")
    )
    return answer_data


def testcase() -> int:
    results = list(map(len, parse_answers_file("inputs/06testcase.txt")))
    for x, y in zip(results, (3, 3, 3, 1, 1)):
        assert x == y
    return sum(results)


def num_all_answered(count, item):
    return len([x for x in item if item[x] == count])


def main() -> int:
    return sum(map(len, parse_answers_file("inputs/06.txt")))


def secondary_testcase() -> int:
    return sum(
        starmap(num_all_answered, parse_answers_with_totals("inputs/06testcase.txt"))
    )


def secondary() -> int:
    return sum(starmap(num_all_answered, parse_answers_with_totals("inputs/06.txt")))
