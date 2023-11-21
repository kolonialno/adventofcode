import itertools
from pathlib import Path

map = [
    list(map(int, row.strip()))
    for row in Path("./input/8.txt").read_text().strip().splitlines()
]
height = len(map)
width = len(map[0])

max_scenic_score = 0
visible_trees = 0

for row_index, row in enumerate(map):
    for column_index, tree in enumerate(row):
        if row_index in (0, height - 1) or column_index in (0, width - 1):
            visible_trees += 1
            continue

        rightwards = [(row_index, column) for column in range(column_index + 1, width)]
        leftwards = [(row_index, column) for column in reversed(range(0, column_index))]
        upwards = [(row, column_index) for row in reversed(range(0, row_index))]
        downwards = [(row, column_index) for row in range(row_index + 1, height)]

        is_visible = False
        scenic_score = 1
        for path in (leftwards, upwards, rightwards, downwards):
            if map[row_index][column_index] > max(map[c[0]][c[1]] for c in path):
                is_visible = True

            length = 0
            for c in path:
                length += 1
                if map[row_index][column_index] <= map[c[0]][c[1]]:
                    break
            scenic_score *= length

        visible_trees += int(is_visible)
        max_scenic_score = max(scenic_score, max_scenic_score)

print(visible_trees)
print(max_scenic_score)
