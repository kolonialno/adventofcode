from queue import PriorityQueue

import numpy as np


def calculate_cost(vertex, local_edge):
    x, x_mod = divmod(vertex[0], local_edge)
    y, y_mod = divmod(vertex[1], local_edge)

    tile_level = x + y
    cost_index = (x_mod, y_mod)

    cost = (grid[cost_index] + tile_level) % 9
    if cost == 0:
        cost = 9
    return cost


def get_adjecents(stop, local_edge, global_edge):
    x, y = stop
    adjecents = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    adjecents = [ # Restrict edges
        a for a in adjecents if (0 <= a[0] < global_edge) and (0 <= a[1] < global_edge)
    ]
    return [(adjecent, calculate_cost(adjecent, local_edge)) for adjecent in adjecents]


# Modified https://gist.github.com/qpwo/cda55deee291de31b50d408c1a7c8515#file-dijkstra-py
def cheapest_path(start, goal, local_edge, global_edge):
    visited = set()
    cost = {start: 0}
    parent = {start: None}
    todo = PriorityQueue()
    todo.put((0, start))

    while not todo.empty():
        _, vertex = todo.get()  # Get lowest cost vertex
        if vertex in visited: # Drain until we find an unvisited vertex
            continue
        visited.add(vertex)
        if vertex == goal:
            return cost[goal]  # Done!
        for neighbor, distance in get_adjecents(vertex, local_edge, global_edge):
            if neighbor in visited:
                continue
            old_cost = cost.get(neighbor, float("inf"))
            new_cost = cost[vertex] + distance
            if new_cost < old_cost:
                todo.put((new_cost, neighbor))
                cost[neighbor] = new_cost
                parent[neighbor] = vertex


with open("inputs/test.txt") as f:
    grid = np.array([list(l) for l in f.read().splitlines()]).astype(int)

start = (0, 0)
local_edge = grid.shape[0]

# Puzzle 1
goal = (local_edge - 1,) * 2
print(f"Answer1: {cheapest_path(start, goal, local_edge, local_edge)}")

# # Puzzle 2
tile_multiplier = 5
goal = (tile_multiplier * local_edge - 1,) * 2
print(f"Answer1: {cheapest_path(start, goal, local_edge, tile_multiplier*local_edge)}")
