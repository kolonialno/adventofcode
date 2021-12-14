from collections import Counter
from functools import cache

with open("inputs/14.txt") as f:
    template = f.readline().strip()
    rules = {}
    f.readline()  # skip a row
    for line in f.read().splitlines():
        a, b = [x.strip() for x in line.split("->")]
        rules[a] = b


@cache
def grow_polymer_pair(pair: str, steps: int):
    if steps == 0:
        return Counter()
    new_letter = rules[pair]
    return (
        grow_polymer_pair(pair[0] + new_letter, steps - 1)
        + Counter(new_letter)
        + grow_polymer_pair(new_letter + pair[1], steps - 1)
    )


def grow_polymers(template: str, steps: int):
    return sum(
        [  # moving window over pairs in string
            grow_polymer_pair(template[i : i + 2], steps)
            for i in range(len(template) - 1)
        ],
        Counter(template),
    )


poly_values = grow_polymers(template, 20).values()
print(f"Answer1: {max(poly_values) - min(poly_values)}")

poly_values = grow_polymers(template, 40).values()
print(f"Answer2: {max(poly_values) - min(poly_values)}")
