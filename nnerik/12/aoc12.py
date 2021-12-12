from collections import defaultdict


def explore(exits, cave, visited, allow_detour):
    if cave == "end":
        return 1
    return sum(
        explore(
            exits, c, visited | {c}, allow_detour and (c.isupper() or c not in visited)
        )
        for c in exits[cave]
        if c != "start" and (allow_detour or c.isupper() or c not in visited)
    )


exits = defaultdict(list)
with open("input.txt") as f:
    for line in f:
        a, b = line.strip().split("-")
        exits[a].append(b)
        exits[b].append(a)

print("Part 1:", explore(exits, "start", {"start"}, False))
print("Part 2:", explore(exits, "start", {"start"}, True))
