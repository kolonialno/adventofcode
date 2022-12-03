with open("input.txt") as f:
    data = f.read()

def priority(item: str) -> int:
    c = ord(item)
    if c >= 97:
        return c - 96
    else:
        return c - 64 + 26

def rucksack_priority(rucksack: str) -> int:
    half = int(len(rucksack)/2)
    comp1 = {priority(item) for item in rucksack[:half]}
    comp2 = {priority(item) for item in rucksack[half:]}
    return comp1.intersection(comp2).pop()

def badge(group: list[str]) -> int:
    items1 = {priority(item) for item in group[0]}
    items2 = {priority(item) for item in group[1]}
    items3 = {priority(item) for item in group[2]}
    return items1.intersection(items2).intersection(items3).pop()

rucksacks = data.split("\n")
print(sum([rucksack_priority(r) for r in data.split("\n")]))

print(sum([badge(rucksacks[i:i+3]) for i in range(0, len(rucksacks), 3)]))
