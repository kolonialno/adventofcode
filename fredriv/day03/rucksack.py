def priority(item: str) -> int:
    if item >= 'a':
        return ord(item) - ord('a') + 1
    else:
        return ord(item) - ord('A') + 27

def common(rucksack: str) -> int:
    half = len(rucksack) // 2
    return set(rucksack[:half]).intersection(rucksack[half:]).pop()

def badge(group: list[str]) -> int:
    return set(group[0]).intersection(group[1]).intersection(group[2]).pop()

with open("input.txt") as f:
    data = f.read()
rucksacks = data.split("\n")

print(sum([priority(common(r)) for r in data.split("\n")]))
print(sum([priority(badge(rucksacks[i:i+3])) for i in range(0, len(rucksacks), 3)]))
