import numpy as np

with open("inputs/13.txt") as f:
    fold_rules = []
    x_dots = []
    y_dots = []
    for line in f.read().splitlines():
        if not line:
            continue
        if "=" in line:
            fold_rule = line.split("-")[-1]
            a, b = fold_rule.split("=")
            fold_rules.append(("x" == a[-1], int(b)))
        else:
            x, y = line.split(",")
            x_dots.append(int(x))
            y_dots.append(int(y))

paper = np.zeros((max(y_dots) + 1, max(x_dots) + 1))
paper[y_dots, x_dots] = 1

for should_rotate, slice in fold_rules:
    if should_rotate:
        paper = paper.T

    top = paper[:slice]
    bottom_flipped = np.flipud(paper[slice + 1 :])

    if top.shape[0] - bottom_flipped.shape[0]:
        # Bottom is shorter, add a row of zeros:
        bottom_flipped = np.insert(bottom_flipped, 0, 0, 0)

    paper = top + bottom_flipped

    if should_rotate:
        paper = paper.T

for line in paper.astype(bool):
    print("".join("." if l else "#" for l in line))
