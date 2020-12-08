from itertools import starmap
from math import ceil


def coded_binary_search(input_range, instructions) -> int:
    if len(instructions) == 0:
        return input_range.start
    instruction = instructions[0]

    if instruction in ("F", "L"):
        divisor = len(input_range) // 2
        return coded_binary_search(input_range[:divisor], instructions[1:])
    else:
        divisor = ceil(len(input_range) / 2)
        return coded_binary_search(input_range[divisor:], instructions[1:])


def extract_instructions(filename) -> tuple:
    return ((x[:-4], x[-4:]) for x in open(filename))


def compute_seat_id(row_codes, column_codes) -> int:
    row_value = coded_binary_search(range(0, 127), row_codes) * 8
    column_value = coded_binary_search(range(0, 7), column_codes)
    return row_value + column_value


def testcase() -> int:
    results = list(
        starmap(compute_seat_id, extract_instructions("inputs/05testcase.txt"))
    )
    for result, reference in zip(results, (357, 567, 119, 820)):
        assert result == reference, f"{result}, {reference}"
    return max(results)


def main() -> int:
    return max(sorted(starmap(compute_seat_id, extract_instructions("inputs/05.txt"))))


def secondary() -> int:
    sorted_results = sorted(
        starmap(compute_seat_id, extract_instructions("inputs/05.txt"))
    )
    full_range = range(min(sorted_results), max(sorted_results))
    for x, y in zip(sorted_results, full_range):
        if x != y:
            return y
