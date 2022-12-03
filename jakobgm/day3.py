from pathlib import Path


def priority(char: str) -> int:
    if common < "a":
        return ord(common) - ord("A") + 27
    else:
        return ord(common) - ord("a") + 1


elves = Path("input/3.txt").read_text().strip().splitlines()

priority_sum = 0
for line in elves:
    midpoint = len(line) // 2
    first, second = line[:midpoint], line[midpoint:]
    common_item: str = set(first).intersection(set(second)).pop()
    priority_sum += priority(common_item)
print(priority_sum)

badge_sum = 0
for first, second, third in zip(elves[::3], elves[1::3], elves[2::3]):
    common = set(first).intersection(second).intersection(third).pop()
    badge_sum += priority(common)
print(badge_sum)
