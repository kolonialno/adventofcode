from sys import stdin

world = [list(line.strip()) for line in stdin.readlines()]


def count_trees(slope):
    (x, y) = (0, 0)
    (dx, dy) = slope

    rows, cols = len(world), len(world[0])

    trees = 0
    while y < rows - 1:
        y = y + dy
        x = (x + dx) % cols
        if world[y][x] == "#":
            trees += 1

    return trees


def a():
    slope = (3, 1)
    return count_trees(slope)


def b():
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

    product = 1
    for slope in slopes:
        product *= count_trees(slope)

    return product


print(a())
print(b())
