from pathlib import Path

from networkx import DiGraph, shortest_path

import numpy as np

puzzle = np.genfromtxt("inputs/15.txt", dtype=int, delimiter=1) - 1
for axis in (0, 1):
    puzzle = np.concatenate([puzzle + x for x in range(0, 5)], axis=axis) % 9
puzzle += 1

rows, cols = puzzle.shape
puzzle = np.pad(puzzle, pad_width=1, mode="constant", constant_values=10000)
graph = DiGraph()
for row in range(1, rows + 1):
    for col in range(1, cols + 1):
        targets = ((row + 1, col), (row, col + 1), (row - 1, col), (row, col - 1))
        for target in targets:
            graph.add_edge(
                (row, col),
                target,
                weight=puzzle[target[0], target[1]]
            )

solution = shortest_path(graph, (1, 1), (rows, cols), weight="weight")
print(sum(puzzle[node] for node in solution[1:]))
