import numpy as np


def count_visible_trees(array, h):
    if array.max() < h:
        return len(array)
    else:
        return (array >= h).argmax() + 1


with open("input.txt") as f:
    grid = np.array([[int(c) for c in r] for r in f.read().strip().split("\n")])

rows, cols = grid.shape
visible_trees = rows * 2 + (cols - 2) * 2
scenic_score = 0

for r in range(1, rows - 1):
    for c in range(1, cols - 1):
        tree_height = grid[r, c]
        directions = [
            grid[:r, c][::-1],
            grid[r + 1 :, c],
            grid[r, :c][::-1],
            grid[r, c + 1 :],
        ]

        if any(np.array([d.max() for d in directions]) < tree_height):
            visible_trees += 1

        current_scenic_score = np.prod([count_visible_trees(d, tree_height) for d in directions])
        if current_scenic_score > scenic_score:
            scenic_score = current_scenic_score

print(visible_trees)
print(scenic_score)
