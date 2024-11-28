def solve(floor):
    basins = [[None for _ in floor[0]] for _ in floor]
    width, height = len(floor[0]), len(floor)

    def adjacents(x, y):
        return {
            (x, y)
            for x, y in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
            if 0 <= x < width and 0 <= y < height
        }

    def fill(x, y, value):
        basins[y][x] = value
        for x2, y2 in adjacents(x, y):
            if basins[y2][x2] is None and floor[y2][x2] < 9:
                fill(x2, y2, value)

    lows = [
        (x, y)
        for y, line in enumerate(floor)
        for x, h in enumerate(line)
        if h < min(floor[y2][x2] for x2, y2 in adjacents(x, y))
    ]

    for low in lows:
        fill(*low, low)

    return lows, sorted([b for row in basins for b in row].count(low) for low in lows)


with open("input.txt") as f:
    floor = [[int(h) for h in line.strip()] for line in f]

lows, sizes = solve(floor)

print("Part 1:", sum(floor[y][x] + 1 for x, y in lows))
print("Part 2:", sizes[-1] * sizes[-2] * sizes[-3])
