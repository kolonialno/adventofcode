# Read input
map = open(0).read().strip().split("\n")

x, y = -1, -1
width, height = len(map[0]), len(map)
for y, row in enumerate(map):
    if (x := row.find("^")) >= 0:
        break

start = (x, y)
direction = 0
directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

dx, dy = directions[direction]
visited = set()
while True:
    visited.add((x, y))
    if not 0 <= x + dx < width or not 0 <= y + dy < height:
        break
    if map[y + dy][x + dx] == "#":
        direction = (direction + 1) % 4
        dx, dy = directions[direction]
        continue
    x += dx
    y += dy

print("Part 1:", len(visited))

count = 0
for obstacle_x in range(width):
    for obstacle_y in range(height):
        if (obstacle_x, obstacle_y) == start:
            continue
        print("Checking", obstacle_x, obstacle_y)
        x, y = start
        direction = 0
        dx, dy = directions[direction]
        moves = set()
        while True:
            if (x, y, direction) in moves:
                count += 1
                break
            moves.add((x, y, direction))
            if not 0 <= x + dx < width or not 0 <= y + dy < height:
                break
            if map[y + dy][x + dx] == "#" or (x + dx, y + dy) == (
                obstacle_x,
                obstacle_y,
            ):
                direction = (direction + 1) % 4
                dx, dy = directions[direction]
                continue
            x += dx
            y += dy


print("Part 2:", count)
