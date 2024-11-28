import itertools
from pathlib import Path

import networkx as nx
import numpy as np

problem = Path("./input/18.txt").read_text().strip().splitlines()
cubes = np.array([list(map(int, row.split(","))) for row in problem])

# Task 1
surface_area = 0
for cube in cubes:
    distances = np.sum(np.abs(cubes - cube), axis=1)
    uncovered_sides = 6 - (distances == 1).sum()
    surface_area += uncovered_sides
print(surface_area)

# Task 2
x_min, y_min, z_min = cubes.min(axis=0)
x_max, y_max, z_max = cubes.max(axis=0)
grid = {
    coordinate
    for coordinate in itertools.product(
        range(x_min - 1, x_max + 2),
        range(y_min - 1, y_max + 2),
        range(z_min - 1, z_max + 2),
    )
}
gaps = grid - set(map(tuple, cubes))


def adjacent_gaps(cube):
    for dimension, direction in itertools.product([0, 1, 2], [-1, 1]):
        neighbor = np.array(cube)
        neighbor[dimension] += direction
        neighbor = tuple(neighbor)
        if neighbor in gaps:
            yield neighbor


graph = nx.Graph()
graph.add_edges_from(
    [(gap, adjacent_gap) for gap in gaps for adjacent_gap in adjacent_gaps(gap)]
)

outside = (x_min - 1, y_min - 1, z_min - 1)
outside_graph = nx.induced_subgraph(
    graph,
    nx.node_connected_component(graph, outside),
)
outside_surface_area = 0
for cube in cubes:
    for adjacent_gap in adjacent_gaps(cube):
        if adjacent_gap in outside_graph:
            outside_surface_area += 1

print(outside_surface_area)
