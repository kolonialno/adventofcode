from collections import Counter, defaultdict

connections = defaultdict(list)
with open("inputs/12.txt") as f:
    for line in f.read().splitlines():
        print(line)
        a, b = line.split("-")
        if b != "start":
            connections[a].append(b)
        if a != "start":
            connections[b].append(a)


def traverse(path, possible_connections, max_visits=1):
    for c in possible_connections:
        small_cave_count = Counter(p for p in path if p.islower())
        if (
            small_cave_count[c] >= max_visits
            or list(small_cave_count.values()).count(2) > 1
        ):
            continue
        elif c == "end":
            paths.append(path + [c])
        else:
            traverse(path + [c], connections[c], max_visits)


paths = []
traverse(["start"], connections["start"])
print(f"Answer1: {len(paths)}")

paths = []
traverse(["start"], connections["start"], max_visits=2)
print(f"Answer2: {len(paths)}")
