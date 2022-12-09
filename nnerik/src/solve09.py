def sign(n):
    return (n > 0) - (n < 0)


def move_tail(hx, hy, tx, ty):
    x, y = tx, ty
    if not -1 <= hx - tx <= 1:
        y = ty + sign(hy - ty)
        x = hx + sign(tx - hx)
    if not -1 <= hy - ty <= 1:
        x = tx + sign(hx - tx)
        y = hy + sign(ty - hy)
    return (x, y)


def moves(data):
    for line in data.splitlines():
        d, n = line.split()
        yield {"L": (-1, 0), "R": (1, 0), "U": (0, -1), "D": (0, 1)}[d] + (int(n),)


def solve(data, length):
    knots = [(0, 0)] * length
    visits = set()
    for dx, dy, n in moves(data):
        for _ in range(n):
            hx, hy = knots[0]
            knots[0] = (hx + dx, hy + dy)
            for i in range(length - 1):
                hx, hy = knots[i]
                x, y = knots[i + 1]
                knots[i + 1] = move_tail(hx, hy, x, y)
            visits.add(knots[-1])
    return len(visits)


def solve1(data):
    return solve(data, 2)


def solve2(data):
    return solve(data, 10)
