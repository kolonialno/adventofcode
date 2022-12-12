from collections import defaultdict
from pathlib import Path

from networkx import DiGraph, shortest_path, NetworkXNoPath


# Create map and current position
problem = [list(row) for row in Path("./input/12.txt").read_text().strip().splitlines()]
map = {}
current_position = None
goal = None
for row_index, row in enumerate(problem):
    for column_index, char in enumerate(row):
        if char == "S":
            current_position = row_index, column_index
            char = "a"
        if char == "E":
            goal = row_index, column_index
            char = "z"

        map[(row_index, column_index)] = ord(char) - ord("a") + 1

graph = DiGraph()
for (row_index, column_index), elevation in map.items():
    for step in (
        (row_index - 1, column_index),
        (row_index + 1, column_index),
        (row_index, column_index + 1),
        (row_index, column_index - 1),
    ):
        try:
            target_value = map[step]
            if target_value - elevation <= 1:
                graph.add_edge(
                    (row_index, column_index),
                    step,
                )
        except KeyError:
            continue

print(len(shortest_path(graph, current_position, goal)) - 1)

shortest_climb = float("inf")
for start in (start for start, elevation in map.items() if elevation == 1):
    try:
        solution = shortest_path(graph, start, goal)
        shortest_climb = min(shortest_climb, len(solution) - 1)
    except NetworkXNoPath:
        continue

print(shortest_climb)
