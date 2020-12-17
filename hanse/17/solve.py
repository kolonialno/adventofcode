from sys import stdin
from itertools import product

grid = [list(line.strip()) for line in stdin]


def generate_neighbors(coordinate, dimensions):
    for deltas in (
        delta
        for delta in product([-1, 0, 1], repeat=dimensions)
        if delta != tuple([0] * dimensions)
    ):
        yield tuple(a + b for (a, b) in zip(coordinate, deltas))


def next_state(prev_state, dimensions):
    state = prev_state.copy()
    active = 0

    for coordinate in prev_state.keys():
        neighbors = [cube for cube in generate_neighbors(coordinate, dimensions)]

        for cell in neighbors:
            if cell not in state:
                state[cell] = "."

        active_neighbors = len(
            [cube for cube in neighbors if prev_state.get(cube) == "#"]
        )

        if prev_state[coordinate] == "#":
            state[coordinate] = "#" if active_neighbors in (2, 3) else "."
        elif prev_state[coordinate] == ".":
            state[coordinate] = "#" if active_neighbors == 3 else "."

        if state[coordinate] == "#":
            active += 1

    return state, active


def count_active_cubes(dimensions):
    state = {}

    max_y = len(grid)
    max_x = len(grid[0])

    xy = [range(-1, max_y + 1), range(-1, max_x + 1)]
    zw = [(-1, 0, 1)] * (dimensions - len(xy))

    for coordinate in product(*(xy + zw)):
        state[coordinate] = "."

    for y in range(max_y):
        for x in range(max_x):
            coordinate = tuple([x, y] + [0] * (dimensions - len(xy)))
            state[coordinate] = grid[y][x]

    current_state = state
    current_active = 0

    for _ in range(6):
        current_state, current_active = next_state(current_state, dimensions)

    return current_active


def a():
    return count_active_cubes(3)


def b():
    return count_active_cubes(4)


print(a())
print(b())
