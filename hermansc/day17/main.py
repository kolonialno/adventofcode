from itertools import product

def neighbours(curr):
    for c in [coord for coord in product(*(range(d-1, d+2) for d in curr)) if coord != curr]:
        yield c

def coordinates(thresholds):
    for c in product(*(range(r[0]-1, r[1]+2) for r in thresholds)):
        yield c

def solve(dimensions=3):
    coords = {
        ((0, ) * (dimensions - 2) + (ridx, cidx))
        for ridx, row in enumerate(open("grid.txt").read().strip().split("\n"))
        for cidx, col in enumerate(row) if col == "#"
    }

    for cycle in range(6):
        new = set()
        thresholds = [(min(c[i] for c in coords), max(c[i] for c in coords)) for i in range(dimensions)]

        for ca in coordinates(thresholds):
            active = len([c for c in neighbours(ca) if c in coords])
            if (
                (ca not in coords and active == 3) # Inactive, 3 neighbours
                or (ca in coords and 2 <= active <= 3) # Active, 2-3 neighbours
            ):
                new.add(ca)
        coords = new

    return coords

# Part 1
print(len(solve()))

# Part 2
print(len(solve(dimensions=4)))
