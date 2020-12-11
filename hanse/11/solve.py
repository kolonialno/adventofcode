from sys import stdin

grid = [list(line.strip()) for line in stdin]


def generate_adjacent(grid, x, y, infinite=False):
    deltas = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

    ks = [1] if not infinite else range(1, len(grid))

    for (dx, dy) in deltas:
        for k in ks:
            _x = x + dx * k
            _y = y + dy * k

            if not (_x >= 0 and _x < len(grid[0]) and _y >= 0 and _y < len(grid)):
                continue

            yield (_x, _y)

            if grid[_y][_x] != ".":
                break


def next_state(prev_grid, threshold, infinite):
    grid = [row[:] for row in prev_grid]
    total_occupied = 0

    for y in range(len(prev_grid)):
        for x in range(len(prev_grid[0])):
            if prev_grid[y][x] == ".":
                continue

            occupied = len(
                [
                    (ax, ay)
                    for (ax, ay) in generate_adjacent(prev_grid, x, y, infinite)
                    if prev_grid[ay][ax] == "#"
                ]
            )

            if prev_grid[y][x] == "L" and occupied == 0:
                grid[y][x] = "#"
            elif prev_grid[y][x] == "#" and occupied >= threshold:
                grid[y][x] = "L"

            if grid[y][x] == "#":
                total_occupied += 1

    return grid, total_occupied


def solve(threshold, infinite=False):
    current_grid = grid
    current_occupied = 0

    while True:
        current_grid, occupied = next_state(current_grid, threshold, infinite)

        if occupied == current_occupied:
            return occupied

        current_occupied = occupied

    return -1


def a():
    return solve(4, False)


def b():
    return solve(5, True)


print(a())
print(b())
