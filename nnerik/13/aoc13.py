def fold(p, axis, n):
    x, y = p
    if axis == "x" and x < n or axis == "y" and y < n:
        return (x, y)
    if axis == "x":
        return (2 * n - x, y)
    if axis == "y":
        return (x, 2 * n - y)


points = set()
with open("input.txt") as f:
    while line := next(f).strip():
        points.add(tuple(map(int, line.split(","))))
    folds = [
        (axis, int(n)) for line in f for axis, n in (line[11:].strip().split("="),)
    ]

# Part 1
print("Part 1:", len({fold(p, *(folds[0])) for p in points}))

# Part 2
for axis, n in folds:
    points = {fold(p, axis, n) for p in points}
print("\nPart 2:")
for y in range(max(p[1] for p in points) + 1):
    for x in range(max(p[0] for p in points) + 1):
        if (x, y) in points:
            print("#", end="")
        else:
            print(" ", end="")
    print()
