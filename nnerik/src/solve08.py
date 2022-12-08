def is_visible(rows, y0, x0):
    width = len(rows[0])
    height = len(rows)
    tree_height = rows[y0][x0]
    for delta in ((1, 0), (-1, 0), (0, 1), (0, -1)):
        (x, y) = (x0, y0)
        while True:
            (x, y) = (x + delta[0], y + delta[1])
            if not (0 <= x < width and 0 <= y < height):
                return True
            if rows[y][x] >= tree_height:
                break
    return False


def scenic_score(rows, y0, x0):
    width = len(rows[0])
    height = len(rows)
    tree_height = rows[y0][x0]
    score = 1
    for delta in ((1, 0), (-1, 0), (0, 1), (0, -1)):
        (x, y) = (x0, y0)
        directional_score = 0
        while True:
            (x, y) = (x + delta[0], y + delta[1])
            if not (0 <= x < width and 0 <= y < height):
                break
            directional_score += 1
            if tree_height <= rows[y][x]:
                break
        score *= directional_score
    return score


def forest_mapper(f, data):
    rows = data.splitlines()
    return (f(rows, x, y) for y in range(len(rows)) for x in range(len(rows[0])))


def solve1(data):
    return sum(forest_mapper(is_visible, data))


def solve2(data):
    return max(forest_mapper(scenic_score, data))
