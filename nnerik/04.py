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
        }
        for grid in grids
    ]

    def scores():
        for n in numbers:
            for board in filter(lambda b: b, boards):
                if n in board["numbers"]:
                    board["numbers"].remove(n)
                    for i, line in enumerate(board["lines"]):
                        if n in line:
                            line.remove(n)
                            if not line:
                                yield reduce(lambda a, b: a + b, board["numbers"]) * n
                                board.clear()
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
