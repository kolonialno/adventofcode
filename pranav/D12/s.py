
from collections import defaultdict

with open("input.txt") as f:
    lines = f.readlines()

class Cave:
    def __init__(self) -> None:
        self.paths = defaultdict(list)
    
    def add_path(self, cave1, cave2):
        self.paths[cave1].append(cave2)
        self.paths[cave2].append(cave1)
    
    def __str__(self):
        return str(self.paths)

def traverse(cave, start, visited, path, paths, with_twice=False):
    visited.append(start)
    path.append(start)
    if start == 'end':
        paths.append(path)
    for c in cave.paths[start]:
        if c not in visited or c.isupper():
            traverse(cave, c, visited.copy(), path.copy(), paths, with_twice=with_twice)
        elif with_twice and c != 'start' and c != 'end':
            traverse(cave, c, visited.copy(), path.copy(), paths, with_twice=False)

def create_cave():
    paths = [l.strip().split('-') for l in lines]
    cave = Cave()
    for x, y in paths:
        cave.add_path(x, y)
    return cave

def func1():
    c = create_cave()
    paths = []
    traverse(c, 'start', [], [], paths)

    return len(paths)

def func2():
    c = create_cave()
    paths = []
    traverse(c, 'start', [], [], paths, with_twice=True)

    return len(paths)

# print(func1())
print(func2())