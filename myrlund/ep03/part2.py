from functools import reduce
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


gear_index_to_adjacent_indexes = {
    (j, i): {(x, y) for (x, y) in with_neighbors(j, i)}
    for i, line in enumerate(lines)
    for j, symbol in enumerate(line.strip())
    if symbol == "*"
}

numbers_by_digit_indexes = {
    (j, i): int(match.group(0))
    for i, line in enumerate(lines)
    for match in re.finditer(r"\d+", line)
    for j in range(match.start(), match.end())
}

gear_numbers = [
    {
        numbers_by_digit_indexes[adjacent_index]
        for adjacent_index in adjacent_indexes
        if adjacent_index in numbers_by_digit_indexes
    }
    for adjacent_indexes in gear_index_to_adjacent_indexes.values()
]

gear_values = [
    reduce(lambda x, y: x * y, nums) for nums in gear_numbers if len(nums) == 2
]
print(sum(gear_values))
