from collections import defaultdict


def solve(start, rules, times):
    pairs = defaultdict(int)
    for i in range(len(start) - 1):
        pairs[start[i : i + 2]] += 1

    for i in range(times):
        new_pairs = defaultdict(int)
        for k, v in pairs.items():
            new_pairs[k[0] + rules[k]] += v
            new_pairs[rules[k] + k[1]] += v
        pairs = new_pairs

    elements = defaultdict(int)
    elements[start[-1]] += 1
    for k, v in pairs.items():
        elements[k[0]] += v

    return max(elements.values()) - min(elements.values())


with open("input.txt") as f:
    start = f.readline().strip()
    f.readline()
    rules = {pair: elm for pair, elm in (line.strip().split(" -> ") for line in f)}

print("Part 1:", solve(start, rules, 10))
print("Part 2:", solve(start, rules, 40))
