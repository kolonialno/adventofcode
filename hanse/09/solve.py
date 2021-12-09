from sys import stdin

grid = [[int(x) for x in line.strip()] for line in stdin]


def generate_adjacent(grid, x, y):
    for dx, dy in [(-1, 0), (0, -1), (0, 1), (1, 0)]:
        _x, _y = x + dx, y + dy

        if not (_x >= 0 and _x < len(grid[0]) and _y >= 0 and _y < len(grid)):
            continue

        yield (_x, _y)


def get_low_points():
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            adjacent = generate_adjacent(grid, x, y)
            if all(grid[y][x] < grid[v][u] for u, v in adjacent):
                yield (x, y)


def a():
    return sum(grid[y][x] + 1 for x, y in get_low_points())


def b():
    low_points = get_low_points()

    basins = []
    for low_point in low_points:
        basin = 0
        visited = set()
        queue = [low_point]
        while queue:
            x, y = queue.pop(0)
            adjacent = generate_adjacent(grid, x, y)

            for (x1, y1) in adjacent:
                if (x1, y1) in visited:
                    continue

                visited.add((x1, y1))

                if grid[y1][x1] < 9:
                    basin += 1
                    queue.append((x1, y1))

        basins.append(basin)

    product = 1
    for x in sorted(basins, reverse=True)[:3]:
        product *= x
    return product


print(a())
print(b())
