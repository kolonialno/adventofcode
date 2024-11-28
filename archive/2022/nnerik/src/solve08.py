def score_function_factory(score_fn, subscore_fn, edge_fn):
    def score_function(rows, y0, x0):
        width, height = len(rows[0]), len(rows)
        tree_height = rows[y0][x0]
        score = None
        for delta in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            (x, y) = (x0, y0)
            subscore = None
            while True:
                (x, y) = (x + delta[0], y + delta[1])
                if not (0 <= x < width and 0 <= y < height):
                    subscore = edge_fn(subscore)
                    break
                subscore = subscore_fn(subscore)
                if tree_height <= rows[y][x]:
                    break
            score = score_fn(score, subscore)
        return score

    return score_function


def forest_mapper(f, forest):
    rows = forest.splitlines()
    return (f(rows, x, y) for y in range(len(rows)) for x in range(len(rows[0])))


def solve1(data):
    score_fn = score_function_factory(
        lambda s, u: s or u,
        lambda _: False,
        lambda _: True,
    )
    return sum(forest_mapper(score_fn, data))


def solve2(data):
    score_fn = score_function_factory(
        lambda s, u: (1 if s is None else s) * u,
        lambda u: 1 if u is None else u + 1,
        lambda u: u or 0,
    )
    return max(forest_mapper(score_fn, data))
