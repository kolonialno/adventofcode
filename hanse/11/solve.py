from copy import deepcopy
from sys import stdin

input_grid = [[int(x) for x in line.strip()] for line in stdin]


def generate_adjacent(grid, x, y):
    deltas = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ]

    for dx, dy in deltas:
        _x, _y = x + dx, y + dy

        if not (_x >= 0 and _x < len(grid[0]) and _y >= 0 and _y < len(grid)):
            continue

        yield (_x, _y)


def a():
    grid = deepcopy(input_grid)
    flashes = 0
    has_flashed = set()

    def maybe_flash(x, y):
        nonlocal flashes, has_flashed
        if (x, y) in has_flashed:
            return

        if grid[y][x] > 9:
            has_flashed.add((x, y))
            flashes += 1
            for (x1, y1) in generate_adjacent(grid, x, y):
                grid[y1][x1] += 1
                maybe_flash(x1, y1)

    for _ in range(100):
        has_flashed.clear()

        for y in range(len(grid)):
            for x in range(len(grid[0])):
                grid[y][x] += 1
                maybe_flash(x, y)

        for (x, y) in has_flashed:
            grid[y][x] = 0

    return flashes


def b():
    grid = deepcopy(input_grid)
    has_flashed = set()

    def maybe_flash(x, y):
        nonlocal has_flashed
        if (x, y) in has_flashed:
            return

        if grid[y][x] > 9:
            has_flashed.add((x, y))
            for (x1, y1) in generate_adjacent(grid, x, y):
                grid[y1][x1] += 1
                maybe_flash(x1, y1)

    for step in range(1, 1000):
        has_flashed.clear()

        for y in range(len(grid)):
            for x in range(len(grid[0])):
                grid[y][x] += 1
                maybe_flash(x, y)

        for (x, y) in has_flashed:
            grid[y][x] = 0

        all_flashes = True
        for y in range(len(grid)):
            for x in range(len(grid[0])):
                if grid[y][x] != 0:
                    all_flashes = False
                    break

        if all_flashes:
            return step

    return None


print(a())
print(b())
