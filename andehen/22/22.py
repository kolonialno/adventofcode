import sys
import numpy as np
import re


def parse_map(map_txt: str):

    map_txt_lines = map_txt.split("\n")
    max_width = max([len(l) for l in map_txt_lines])

    map = np.array([list(s.ljust(max_width)) for s in map_txt_lines])

    return map


def parse_path(path_txt: str) -> list[tuple[str, int]]:
    path_regex = r"([SRL]+)(\d+)"
    return [(d, int(s)) for d, s in re.findall(path_regex, "S" + path_txt)]


def get_first_tile(array) -> int:
    return np.where(array != " ")[0][0]


def get_last_tile(array) -> int:
    return np.where(array != " ")[0][-1]


def is_outside(map, pos) -> bool:
    n_rows, n_cols = map.shape

    # We are  out of bounds
    if (pos[0] < 0) or (pos[0] >= n_rows) or (pos[1] < 0) or (pos[1] >= n_cols):
        return True
    # or it's "outside"
    return map[pos] == " "


def get_wrapped_pos(
    current_pos: tuple[int, int], facing: int, cube_size: int
) -> tuple[tuple[int, int], int]:
    row_side, col_side = get_side(current_pos, cube_size)

    match [row_side, col_side, facing]:

        case [0, 1, 2]:
            facing = 0
            row_side = 2
            col_side = 0
            col_offset = col_side * cube_size
            row_offset = row_side * cube_size

            row = row_offset + (cube_size - 1) - current_pos[0] % cube_size
            col = col_offset


        case [0, 1, 3]:
            facing = 0
            row_side = 3
            col_side = 0
            col_offset = col_side * cube_size
            row_offset = row_side * cube_size

            row = row_offset + current_pos[1] % cube_size
            col = col_offset

            return (row, col), facing

        case [0, 2, 3]:
            facing = 3
            row_side = 3
            col_side = 0
            col_offset = col_side * cube_size
            row_offset = row_side * cube_size

            row = row_offset + (cube_size - 1)
            col = col_offset + current_pos[1] % cube_size

            return (row, col), facing

        case [0, 2, 0]:
            facing = 2
            row_side = 2
            col_side = 1
            col_offset = col_side * cube_size
            row_offset = row_side * cube_size

            row = row_offset + (cube_size - 1) - current_pos[0] % cube_size
            col = col_offset + (cube_size - 1)

        case [0, 2, 1]:
            facing = 2
            row_side = 1
            col_side = 1
            col_offset = col_side * cube_size
            row_offset = row_side * cube_size

            row = row_offset + current_pos[1] % cube_size
            col = col_offset + (cube_size - 1)

        case [1, 1, 0]:
            facing = 3
            row_side = 0
            col_side = 2
            col_offset = col_side * cube_size
            row_offset = row_side * cube_size

            row = row_offset + cube_size - 1
            col = col_offset + current_pos[0] % cube_size

        case [1, 1, 2]:
            facing = 1
            row_side = 2
            col_side = 0
            row_offset = row_side * cube_size
            col_offset = col_side * cube_size
            row = row_offset
            col = col_offset + current_pos[0] % cube_size

        case [2, 0, 3]:
            facing = 0
            row_side = 1
            col_side = 1
            row_offset = row_side * cube_size
            col_offset = col_side * cube_size

            row = row_offset + current_pos[1] % cube_size
            col = col_offset

        case [2, 0, 2]:
            facing = 0
            row_side = 0
            col_side = 1
            row_offset = row_side * cube_size
            col_offset = col_side * cube_size

            col = col_offset
            row = row_offset + (cube_size - 1) - current_pos[0] % cube_size

        case [2, 1, 0]:
            facing = 2
            row_side = 0
            col_side = 2
            row_offset = row_side * cube_size
            col_offset = col_side * cube_size

            col = col_offset + (cube_size - 1)
            row = row_offset + (cube_size - 1) - current_pos[0] % cube_size

        case [2, 1, 1]:
            facing = 2
            row_side = 3
            col_side = 0

            row_offset = row_side * cube_size
            col_offset = col_side * cube_size

            row = row_offset + current_pos[1] % cube_size
            col = col_offset + (cube_size - 1)

        case [3, 0, 0]:
            facing = 3
            row_side = 2
            col_side = 1
            row_offset = row_side * cube_size
            col_offset = col_side * cube_size

            row = row_offset + (cube_size - 1)
            col = col_offset + current_pos[0] % cube_size

        case [3, 0, 1]:
            facing = 1
            row_side = 0
            col_side = 2
            row_offset = row_side * cube_size
            col_offset = col_side * cube_size

            row = row_offset
            col = col_offset + current_pos[1] % cube_size

        case [3, 0, 2]:
            facing = 1
            row_side = 0
            col_side = 1
            row_offset = row_side * cube_size
            col_offset = col_side * cube_size

            row = row_offset
            col = col_offset + current_pos[0] % cube_size

        case _:
            raise Exception("Unknown cube wrapping")

    return (row, col), facing


def get_new_pos(
    map, current_pos: tuple[int, int], facing: int, cube_size=None
) -> tuple[tuple[int, int], int]:

    match facing:
        case 0:
            dr = 0
            dc = 1
        case 2:
            dr = 0
            dc = -1
        case 3:
            dr = -1
            dc = 0
        case 1:
            dr = 1
            dc = 0
        case _:
            raise Exception("Unkown direction")

    new_pos = (current_pos[0] + dr, current_pos[1] + dc)

    if not is_outside(map, new_pos):
        return new_pos, facing

    # Otherwise we wrap around
    if cube_size is None:
        match facing:
            case 0:
                return (current_pos[0], get_first_tile(map[current_pos[0], :])), facing
            case 2:
                return (current_pos[0], get_last_tile(map[current_pos[0], :])), facing
            case 1:
                return (get_first_tile(map[:, current_pos[1]]), current_pos[1]), facing
            case 3:
                return (get_last_tile(map[:, current_pos[1]]), current_pos[1]), facing
    else:
        return get_wrapped_pos(current_pos, facing, cube_size)


def get_new_facing(current_facing, rotation) -> int:
    match rotation:
        case "R":
            return (current_facing + 1) % 4
        case "L":
            return (current_facing - 1) % 4
        case "S":
            return current_facing
        case _:
            raise Exception("Unkown direction")


def get_side(pos, cube_size) -> tuple[int, int]:
    row_side = pos[0] // cube_size
    col_side = pos[1] // cube_size
    return row_side, col_side


def get_end_pos(map_txt, path_txt, cube=False) -> tuple[tuple[int, int], int]:

    map = parse_map(map_txt)
    path = parse_path(path_txt)

    cube_size = max(map.shape) // 4 if cube else None

    # Start at top left
    start_row = 0
    start_col = get_first_tile(map[start_row, :])
    current_pos = (start_row, start_col)

    # Start facing right
    facing = 0

    for rotation, steps in path:
        facing = get_new_facing(facing, rotation)
        for _ in range(1, steps + 1):
            new_pos, new_facing = get_new_pos(
                map, current_pos, facing, cube_size=cube_size
            )

            # If we hit a wall we stop
            if map[new_pos] == "#":
                break
            # Otherwise move to next pos
            current_pos = new_pos
            facing = new_facing

    return current_pos, facing


def get_final_password(pos, facing):
    return 1000 * (pos[0] + 1) + 4 * (pos[1] + 1) + facing


map_txt, path_txt = sys.stdin.read().split("\n\n")

final_pos, final_facing = get_end_pos(map_txt, path_txt)
print(get_final_password(final_pos, final_facing))

final_pos_cube, final_facing_cube = get_end_pos(map_txt, path_txt, cube=True)
print(get_final_password(final_pos_cube, final_facing_cube))
