def sign(number):
    return (number > 0) - (number < 0)


def create_cave(data):
    cave = set()
    depth = 0
    for line in data.splitlines():
        points = (
            (int(x), int(y))
            for x, y in (point.split(",") for point in line.split(" -> "))
        )
        x, y = next(points)
        cave.add((x, y))
        while point := next(points, None):
            dx, dy = (sign(point[0] - x), sign(point[1] - y))
            while point != (x, y):
                x, y = x + dx, y + dy
                cave.add((x, y))
                depth = max(depth, y)
    return cave, depth


def add_sand(cave, depth, has_floor=False):
    x, y = 500, 0
    while y < depth:
        if (x, y + 1) not in cave:
            y += 1
        elif (x - 1, y + 1) not in cave:
            x, y = x - 1, y + 1
        elif (x + 1, y + 1) not in cave:
            x, y = x + 1, y + 1
        else:
            return (x, y)
    return (x, y) if has_floor else None


def solve1(data):
    cave, depth = create_cave(data)
    count = 0
    while sand := add_sand(cave, depth):
        count += 1
        cave.add(sand)
    return count


def solve2(data):
    cave, depth = create_cave(data)
    count = 0
    while sand := add_sand(cave, depth + 1, True):
        count += 1
        cave.add(sand)
        if sand == (500, 0):
            return count
