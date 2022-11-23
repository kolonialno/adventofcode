from collections import Counter
from functools import cache
from pathlib import Path

template, pair_insertions = Path("inputs/14.txt").read_text().strip().split("\n\n")
patterns = dict(p.split(" -> ") for p in pair_insertions.split("\n"))

@cache
def insertions(first, second, depth):
    if depth == 40:
        return Counter()
    else:
        insertion = patterns[first + second]
        counter = insertions(first, insertion, depth + 1) + insertions(insertion, second, depth + 1)
        counter[insertion] += 1
        return counter

counter = Counter(template)
for first, second in zip(template[:-1], template[1:]):
    counter += insertions(first, second, depth=0)

print(max(counter.values()) - min(counter.values()))
