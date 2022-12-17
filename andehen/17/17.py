from typing_extensions import TypeAlias


WIDTH = 7
FREE_LEVELS = 3
Level: TypeAlias = list[str]
Grid: TypeAlias = list[Level]
RockPosition: TypeAlias = list[tuple[int, int]]


def empty_level() -> list[str]:
    return ["."] * WIDTH


def is_empty_level(level: Level) -> bool:
    return level == empty_level()


def get_empty_grid(n: int) -> Grid:
    return [empty_level() for _ in range(n)]


def get_num_empty_levels(grid):
    for i, level in enumerate(reversed(grid)):
        if not is_empty_level(level):
            return i
    return len(grid)


def get_rock(rock_id: int) -> RockPosition:
    if rock_id == 0:
        return [(0, 0), (0, 1), (0, 2), (0, 3)]
    if rock_id == 1:
        return [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
    if rock_id == 2:
        return [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)]
    if rock_id == 3:
        return [(0, 0), (1, 0), (2, 0), (3, 0)]
    if rock_id == 4:
        return [(0, 0), (0, 1), (1, 0), (1, 1)]
    else:
        raise Exception("wtf")


def get_rock_grid(rock, x: int) -> Grid:
    rock_height = len(rock)

    grid = get_empty_grid(rock_height)

    for row, level in enumerate(reversed(rock)):
        for col, element in enumerate(level):
            grid[row][x + col] = element

    return grid


def get_rock_position(rock, row_offset, col_offset) -> RockPosition:
    return [(row + row_offset, col + col_offset) for row, col in rock]


def add_falling_rock(grid: Grid, rock) -> tuple[Grid, RockPosition]:
    height_of_rock = max([row for row, _ in rock]) + 1
    num_empty_levels = get_num_empty_levels(grid)

    row_offset, col_offset = len(grid) - num_empty_levels + FREE_LEVELS, 2

    empty_levels_to_add = max(0, row_offset + height_of_rock - len(grid))
    empty_levels = get_empty_grid(empty_levels_to_add)
    initial_rock_position = get_rock_position(rock, row_offset, col_offset)

    return grid + empty_levels, initial_rock_position


def move_rock(rock_position: RockPosition, direction: str) -> RockPosition:

    if direction == ">":
        d_row, d_col = 0, 1
    elif direction == "<":
        d_row, d_col = 0, -1
    elif direction == "Y":
        d_row, d_col = -1, 0
    else:
        raise Exception("Unknown direction")

    new_position = [(row + d_row, col + d_col) for row, col in rock_position]
    return new_position


def is_collision(grid, position) -> bool:

    for row, col in position:
        if row < 0 or col < 0:
            return True
        if col >= WIDTH:
            return True
        if grid[row][col] == "#":
            return True

    return False


def get_new_position(grid, rock_position, direction) -> RockPosition:

    new_rock_position = move_rock(rock_position, direction)

    if is_collision(grid, new_rock_position):
        return rock_position

    return new_rock_position


def add_resting_rock(grid, high_points, rock_position) -> None:
    for row, col in rock_position:
        grid[row][col] = "#"
        high_points[col] = max(high_points[col], row)


def calculate_height(directions, num_rocks):

    num_instructions = len(directions)
    grid = get_empty_grid(1)
    patterns = dict()
    heights = []
    high_points = [-1 for _ in range(WIDTH)]
    direction_counter = 0

    for i in range(num_rocks):
        rock_id = i % 5
        direction_index = direction_counter % num_instructions
        distance_to_ground = tuple(
            len(grid) - get_num_empty_levels(grid) - 1 - hp for hp in high_points
        )
        rock = get_rock(rock_id)
        grid, rock_position = add_falling_rock(grid, rock)
        pattern = (distance_to_ground, rock_id, direction_index)
        exsiting_pattern = patterns.get(pattern)
        height_now = len(grid) - get_num_empty_levels(grid)
        heights.append(height_now)

        if exsiting_pattern:
            pattern_start = exsiting_pattern[0]
            height_then = exsiting_pattern[1]
            cycle_height = height_now - height_then
            cycle_time = i - exsiting_pattern[0]
            rocks_left = num_rocks - i
            cycles_left = rocks_left // cycle_time
            reminder = rocks_left % cycle_time
            reminder_height = heights[pattern_start + reminder] - height_then
            height_at_n = cycles_left * cycle_height + reminder_height + height_now
            return height_at_n
        else:
            patterns[pattern] = (i, height_now)

        is_falling = True
        while is_falling:
            direction = directions[direction_counter % num_instructions]
            rock_position = get_new_position(grid, rock_position, direction)
            direction_counter += 1
            if is_collision(grid, move_rock(rock_position, "Y")):
                add_resting_rock(grid, high_points, rock_position)
                is_falling = False
            else:
                rock_position = get_new_position(grid, rock_position, "Y")


with open("input.txt") as f:
    directions = f.read().strip()


print(calculate_height(directions, 2022))
print(calculate_height(directions, 1000000000000))
