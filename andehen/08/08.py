import numpy as np


def count_visible_trees(array, h):
    if array.max() < h:
        return len(array)
    else:
        return (array >= h).argmax() + 1


with open("input.txt") as f:
    data = f.read()


grid = np.array([[int(c) for c in x] for x in data.strip().split("\n")])


rows, cols = grid.shape

visible_trees = rows * 2 + (cols - 2) * 2
scenic_score = 0

for r in range(1, rows - 1):
    for c in range(1, cols - 1):
        h = grid[r, c]

        upwards = grid[:r, c][::-1]
        downwards = grid[r + 1 :, c]
        leftwards = grid[r, :c][::-1]
        rightwards = grid[r, c + 1 :]

        if any(
            np.array(
                [upwards.max(), downwards.max(), leftwards.max(), rightwards.max()]
            )
            < h
        ):
            visible_trees += 1

        current_scenic_score = np.prod(
            [
                count_visible_trees(a, h)
                for a in [upwards, downwards, leftwards, rightwards]
            ]
        )

        if current_scenic_score > scenic_score:
            scenic_score = current_scenic_score

print(visible_trees)
print(scenic_score)
