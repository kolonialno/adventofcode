from functools import reduce


def board_scores(numbers, grids):
    boards = [list(map(set, grid)) + list(map(set, zip(*grid))) for grid in grids]

    def scores():
        for n in numbers:
            for board in filter(lambda b: b, boards):
                for line in board:
                    if n in line:
                        line.remove(n)
                        if not line:
                            yield sum(
                                reduce(lambda a, b: a.union(b), board) - {n},
                            ) * n
                            board.clear()
                            break

    return scores


with open("input.txt") as f:
    numbers = [int(n.strip()) for n in f.readline().split(",")]
    grids = [
        [[int(n) for n in line.strip().split()] for line in grid.split("\n")]
        for grid in f.read().strip().split("\n\n")
    ]

scores = board_scores(numbers, grids)
print("Part 1:", next(scores()))

for score in scores():
    pass
print("Part 2:", score)
