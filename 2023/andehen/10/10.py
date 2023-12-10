
def get_grid(file):
    with open(file) as f:
        grid = [l for l in f.read().splitlines()]
    return grid


def get_start_pos(grid):
    for i in range(len(grid)):
        for j in range(len(grid[i])):
            if grid[i][j] == "S":
                return i, j

    raise ValueError("No start position found!")


def pipe_to_coordinates(pipe):
    match pipe:
        case "|":
            return (1, 0), (-1, 0)
        case "-":
            return (0, 1), (0, -1)
        case "L":
            return (0, 1), (-1, 0)
        case "J":
            return (-1, 0), (0, -1)
        case "7":
            return (0, -1), (1, 0)
        case "F" | "S":
            return (1, 0), (0, 1)
        case _:
            raise ValueError("Not a pipe!")


def get_next_positions(pos, grid):
    pipe = grid[pos[0]][pos[1]]
    d1, d2 = pipe_to_coordinates(pipe)
    return (pos[0] + d1[0], pos[1] + d1[1]), (pos[0] + d2[0], pos[1] + d2[1])


def get_main_loop_with_distances(grid):
    main_loop = dict()
    to_visit = set([get_start_pos(grid)])
    steps = 0

    while len(to_visit) > 0:
        new_to_visit = set()
        for pos in to_visit:
            main_loop[pos] = steps
            for next_pos in get_next_positions(pos, grid):
                if next_pos not in main_loop:
                    new_to_visit.add(next_pos)
        steps += 1
        to_visit = new_to_visit

    return main_loop


def solve_part_1():
    grid = get_grid("input.txt")
    main_loop = get_main_loop_with_distances(grid)
    print(max(main_loop.values()))


solve_part_1()


# Part 2
def is_in_grid(pos, N, M):
    return 0 <= pos[0] < N and 0 <= pos[1] < M


def get_neighbours(pos):
    neighbours = []
    for d in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
        new_pos = (pos[0] + d[0], pos[1] + d[1])
        neighbours.append(new_pos)
    return neighbours


def get_scaled_up_main_loop_grid(grid, main_loop):
    """
    Map pipes to 3x3 grids.
    """

    new_grid = [[0 for _ in range(len(grid) * 3)] for _ in range(len(grid[0]) * 3)]

    for pos in main_loop:
        pipe = grid[pos[0]][pos[1]]

        i = pos[0] * 3
        j = pos[1] * 3

        if pipe == "|":
            new_grid[i][j + 1] = 1
            new_grid[i + 1][j + 1] = 1
            new_grid[i + 2][j + 1] = 1

        elif pipe == "-":
            new_grid[i + 1][j] = 1
            new_grid[i + 1][j + 1] = 1
            new_grid[i + 1][j + 2] = 1

        elif pipe == "L":
            new_grid[i][j + 1] = 1
            new_grid[i + 1][j + 1] = 1
            new_grid[i + 1][j + 2] = 1

        elif pipe == "J":
            new_grid[i][j + 1] = 1
            new_grid[i + 1][j + 1] = 1
            new_grid[i + 1][j] = 1

        elif pipe == "7":
            new_grid[i + 1][j] = 1
            new_grid[i + 1][j + 1] = 1
            new_grid[i + 2][j + 1] = 1

        elif pipe == "F" or pipe == "S":
            new_grid[i + 1][j + 2] = 1
            new_grid[i + 1][j + 1] = 1
            new_grid[i + 2][j + 1] = 1

    return new_grid


def visit_all_reachable_positions(scaled_grid, start_pos):
    """
    "Pour water" from start_pos until we can't go any further.
    1 means pipe.
    """

    N = len(scaled_grid)
    M = len(scaled_grid[0])

    visited = set()
    to_visit = set([start_pos])

    while len(to_visit) > 0:
        current = to_visit.pop()
        neighbours = [n for n in get_neighbours(current) if is_in_grid(n, N, M)]
        unvisited_neighbours = [n for n in neighbours if n not in visited]

        for neighbour in unvisited_neighbours:
            # Check if we can go there
            if scaled_grid[neighbour[0]][neighbour[1]] != 1:
                to_visit.add(neighbour)

        visited.add(current)

    return visited


def count_full_tiles_visited(scaled_grid):
    """
    Count all 3x3 grids that are fully visited.
    2 is visited.
    """
    N = int(len(scaled_grid) / 3)
    M = int(len(scaled_grid[0]) / 3)

    count = 0
    for i in range(N):
        for j in range(M):
            flattend_values = [scaled_grid[i*3 + k][j*3 + l] for k in range(3) for l in range(3)]
            if all([v == 2 for v in flattend_values]):
                count += 1

    return count


def add_visited_to_grid(grid, visited):
    for pos in visited:
        grid[pos[0]][pos[1]] = 2


def solve_part_2():
    file = "input.txt"
    grid = get_grid(file)
    main_loop = get_main_loop_with_distances(grid)
    scaled_grid = get_scaled_up_main_loop_grid(grid, main_loop)

    start_pos = (225, 225) # Start inside. Found by looking at the image :)
    visited = visit_all_reachable_positions(scaled_grid, start_pos)
    add_visited_to_grid(scaled_grid, visited)
    print(count_full_tiles_visited(scaled_grid))


solve_part_2()
