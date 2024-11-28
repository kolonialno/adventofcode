import re
import sys

with open(sys.argv[1], "r") as f:
    lines = f.readlines()

max_x = len(lines[0])
max_y = len(lines)


def with_neighbors(i, j):
    return [
        (x, y)
        for x in range(max(i - 1, 0), min(i + 2, max_x))
        for y in range(max(j - 1, 0), min(j + 2, max_y))
    ]


def is_symbol(c: str):
    return not c.isnumeric() and c != "."


symbol_adjacent_indexes = {
    (x, y)
    for i, line in enumerate(lines)
    for j, symbol in enumerate(line.strip())
    for (x, y) in with_neighbors(j, i)
    if is_symbol(symbol)
}

numbers_with_digit_indexes = [
    (
        int(match.group(0)),
        [(j, i) for j in range(match.start(), match.end())],
    )
    for i, line in enumerate(lines)
    for match in re.finditer(r"\d+", line)
]
numbers_adjacent_to_symbols = [
    number
    for number, indexes in numbers_with_digit_indexes
    if any(index in symbol_adjacent_indexes for index in indexes)
]

all_numbers = {n for (n, _) in numbers_with_digit_indexes}
numbers_not_adjacent_to_symbols = [
    number
    for number, indexes in numbers_with_digit_indexes
    if all(index not in symbol_adjacent_indexes for index in indexes)
]

print(sum(numbers_adjacent_to_symbols))
