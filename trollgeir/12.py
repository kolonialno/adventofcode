from collections import Counter, defaultdict

connections = defaultdict(list)
with open("inputs/12.txt") as f:
    for line in f.read().splitlines():
        a, b = line.split("-")
        if b != "start":
            connections[a].append(b)
        if a != "start":
            connections[b].append(a)


def traverse(path, possible_connections, max_visits=1):
    paths = []
    small_cave_count = Counter(p for p in path if p.islower())
    can_revisit = list(small_cave_count.values()).count(2) <= 1
    for c in possible_connections:
        if small_cave_count[c] < max_visits and can_revisit:
            if c == "end":
                paths += [path + [c]]
            else:
                paths += traverse(path + [c], connections[c], max_visits)
    return paths


paths = traverse(["start"], connections["start"])
print(f"Answer1: {len(paths)}")

paths = traverse(["start"], connections["start"], max_visits=2)
print(f"Answer2: {len(paths)}")
