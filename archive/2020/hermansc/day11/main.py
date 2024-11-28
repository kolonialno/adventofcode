from itertools import product
from copy import deepcopy

layout = [list(line) for line in open("seats.txt").read().strip().split("\n")]

def evolve(layout, ridiculous=False):
    new_layout = []
    for ridx in range(len(layout)):
        new_row = []
        for cidx in range(len(layout[0])):
            adjecent = []
            for mask in (c for c in product(range(-1, 2), repeat=2) if c != (0, 0)):
                scope = 1
                while ridx + mask[0] * scope in range(len(layout)) and cidx + mask[1] * scope in range(len(layout[0])):
                    val = layout[ridx + mask[0] * scope][cidx + mask[1] * scope]
                    if val != ".":
                        adjecent.append(val)
                        break

                    if not ridiculous:
                        break
                    scope += 1

            lim = 5 if ridiculous else 4
            if layout[ridx][cidx] == "L" and all(s == "L" for s in adjecent):
                new_row.append("#")
            elif layout[ridx][cidx] == "#" and len([s for s in adjecent if s == "#"]) >= lim:
                new_row.append("L")
            else:
                new_row.append(layout[ridx][cidx])

        new_layout.append(new_row)
    return new_layout

def find_stable(layout, ridiculous=False):
    c = 0
    while c < 10000:
        nl = evolve(layout, ridiculous=ridiculous)

        if nl == layout:
            return len([s for r in layout for s in r if s == "#"])
        c += 1
        layout = nl

# Part 1
print(find_stable(layout))

# Part 2
print(find_stable(layout, ridiculous=True))
