from collections import defaultdict
from sys import stdin


lines = [line.strip() for line in stdin]


def analyze():
    cwd: list[str] = []
    sizes: defaultdict[tuple[str], int] = defaultdict(int)

    for line in lines:
        if line.startswith("$ cd"):
            to = line[5:]
            if to == "/":
                cwd = ["/"]
            elif to == "..":
                cwd.pop()
            else:
                cwd.append(to)
        elif line[0].isnumeric():
            size, filename = line.split()
            path = tuple(cwd + [filename])
            for i in range(len(path)):
                sizes[path[:-i]] += int(size)

    return list(sizes.values())


def a():
    sizes = analyze()
    return sum(s for s in sizes if s < 100000)


def b():
    sizes = analyze()
    unused_space = 70000000 - max(s for s in sizes)
    free_up = 30000000 - unused_space
    return min(s for s in sizes if s > free_up)


print(a())
print(b())
