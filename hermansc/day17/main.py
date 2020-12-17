def neighbours(w, z, r, c, wd):
    for wi in range(w+wd[0], w+wd[1]):
        for zi in range(z-1, z+2):
            for ri in range(r-1, r+2):
                for ci in range(c-1, c+2):
                    if wi == w and zi == z and ri == r and ci == c:
                        continue
                    yield (wi, zi, ri, ci)

def coordinates(wr, zr, rr, cr, wd):
    for w in range(wr[0]+wd[0], wr[1]+wd[1]):
        for z in range(zr[0]-1, zr[1]+2):
            for r in range(rr[0]-1, rr[1]+2):
                for c in range(cr[0]-1, cr[1]+2):
                    yield (w, z, r, c)

def solve(coords, w=False):
    cycle = 1
    while cycle <= 6:
        new = set()

        wr = min(c[0] for c in coords), max(c[0] for c in coords)
        zr = min(c[1] for c in coords), max(c[1] for c in coords)
        rr = min(c[2] for c in coords), max(c[2] for c in coords)
        cr = min(c[3] for c in coords), max(c[3] for c in coords)

        wd = (-1, 2) if w else (0, 1)
        for ca in coordinates(wr, zr, rr, cr, wd=wd):
            active = [c for c in neighbours(*ca, wd=wd) if c in coords]

            if (
                (ca not in coords and len(active) == 3) # Inactive, 3 neighbours
                or (ca in coords and 2 <= len(active) <= 3) # Active, 2-3 neighbours
            ):
                new.add(ca)

        coords = new
        cycle += 1

    return coords

coords = set()
for ridx, row in enumerate(open("grid.txt").read().strip().split("\n")):
    for cidx, col in enumerate(row):
        if col == "#":
            coords.add((0, 0, ridx, cidx))

# Part 1
print(len(solve(coords)))

# Part 2
print(len(solve(coords, w=True)))
