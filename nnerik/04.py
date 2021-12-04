from functools import reduce


def read_input(filename):
    with open(filename) as f:
        numbers = [int(n.strip()) for n in f.readline().split(",")]
        grids = [
            [[int(n) for n in line.strip().split()] for line in grid.split("\n")]
            for grid in f.read().strip().split("\n\n")
        ]
        return numbers, grids


def board_scores(input):
    numbers, grids = input
    boards = [
        {
            "numbers": reduce(lambda a, b: a.union(b), grid, set()),
            "lines": list(map(set, grid)) + list(map(set, zip(*grid))),
            "unmarked": [5 for _ in range(10)],
            "live": True,
        }
        for grid in grids
    ]

    def scores():
        for n in numbers:
            for board in filter(lambda b: b["live"], boards):
                if n in board["numbers"]:
                    board["numbers"].remove(n)
                    for i, line in enumerate(board["lines"]):
                        if n in line:
                            board["unmarked"][i] -= 1
                            if not board["unmarked"][i]:
                                yield reduce(lambda a, b: a + b, board["numbers"]) * n
                                board["live"] = False
                                break

    return scores


if __name__ == "__main__":
    input = read_input(__file__[:-3] + ".txt")
    scores = board_scores(input)

    score = next(scores())
    print(f"Part 1: {score}")

    for score in scores():
        pass
    print(f"Part 2: {score}")
