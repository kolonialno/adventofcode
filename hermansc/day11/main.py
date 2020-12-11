from itertools import product
from copy import deepcopy

layout = [list(line) for line in open("seats.txt").read().strip().split("\n")]


def evolve(layout, ridiculous=False):
    def in_bounds(row, column, max_row, max_column):
        return not (ridx + ar < 0 or cidx + ac < 0 or ridx + ar >= max_row or cidx + ac >= max_column)

    new_layout = deepcopy(layout)
    for ridx in range(len(new_layout)):
        row = new_layout[ridx]
        for cidx in range(len(row)):
            seat = new_layout[ridx][cidx]

            adjecent = []
            directions = [c for c in product(range(-1, 2), repeat=2) if c != (0, 0)]
            for mask in directions:
                scope = 1
                ar, ac = (mask[0] * scope, mask[1] * scope)
                while in_bounds(ridx + ar, cidx + ac, max_row=len(layout), max_column=len(row)):

                    if layout[ridx + ar][cidx + ac] != ".":
                        adjecent.append(layout[ridx + ar][cidx + ac])
                        break

                    if not ridiculous:
                        break

                    scope += 1
                    ar, ac = mask[0] * scope, mask[1] * scope

            lim = 5 if ridiculous else 4
            if seat == "L" and all(s == "L" for s in adjecent):
                new_layout[ridx][cidx] = "#"
            elif seat == "#" and len([s for s in adjecent if s == "#"]) >= lim:
                new_layout[ridx][cidx] = "L"

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
