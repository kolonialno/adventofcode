from collections import Counter
import sys

ADJECANT_POSITIONS = [
    (row, col)
    for row in range(-1, 2)
    for col in range(-1, 2)
    if not (row == 0 and col == 0)
]


def parse_grid(grid_txt):
    elves = dict()
    for row, line in enumerate(grid_txt.split("\n")):
        for col, e in enumerate(line):
            if e == "#":
                elf_pos = (row, col)
                elves[(elf_pos)] = None

    return elves


def get_adjecant_positions(pos):
    return [(pos[0] + r, pos[1] + c) for r, c in ADJECANT_POSITIONS]


def get_occupied_adjecant_positions(elves, elf_pos):
    return [pos for pos in get_adjecant_positions(elf_pos) if pos in elves]


def is_free_direction(elf_pos, occupied_adjecant_positions, direction):
    for pos in occupied_adjecant_positions:
        if direction[0] != 0:
            if elf_pos[0] + direction[0] == pos[0]:
                return False
        else:
            if elf_pos[1] + direction[1] == pos[1]:
                return False

    return True


def get_new_position(pos, direction):
    return (pos[0] + direction[0], pos[1] + direction[1])


def get_suggested_new_position(
    elf_pos, occupied_adjecant_positions, direction_priority
):
    for direction in direction_priority:
        if is_free_direction(elf_pos, occupied_adjecant_positions, direction):
            return get_new_position(elf_pos, direction)
    return None


def run_simulation(elves, num_rounds=None):

    elves = elves.copy()
    direction_priority = [(-1, 0), (1, 0), (0, -1), (0, 1)]  # N, S, W, E
    n = 0

    while True:

        n += 1

        # First half round, find suggested positions
        for elf_pos in elves:
            occupied_adjecant_positions = get_occupied_adjecant_positions(
                elves, elf_pos
            )
            if not occupied_adjecant_positions:
                # Elf does nothing
                pass
            else:
                # Suggest to move
                suggested_position = get_suggested_new_position(
                    elf_pos, occupied_adjecant_positions, direction_priority
                )
                elves[elf_pos] = suggested_position

        # Find positions with collisions
        suggested_position_counts = Counter(elves.values())

        elves_to_move = 0
        for elf_pos, suggested_position in elves.copy().items():
            # Make move if no collision
            if suggested_position_counts[suggested_position] == 1:
                elves[suggested_position] = None
                del elves[elf_pos]
                elves_to_move += 1
            else:
                elves[elf_pos] = None

        direction_priority.append(direction_priority.pop(0))

        if (num_rounds and n > num_rounds) or elves_to_move == 0:
            return elves, n


def get_rectangle(elves):

    rows = [pos[0] for pos in elves.keys()]
    cols = [pos[1] for pos in elves.keys()]
    row_min, row_max = min(rows), max(rows)
    col_min, col_max = min(cols), max(cols)

    return row_max - row_min + 1, col_max - col_min + 1


def get_num_empty_tiles(elves, num_rounds):

    elves_after_n, _ = run_simulation(elves, num_rounds)
    rectangle = get_rectangle(elves_after_n)

    return rectangle[0] * rectangle[1] - len(elves)


grid_txt = sys.stdin.read().strip()
elves = parse_grid(grid_txt)

num_empty_tiles = get_num_empty_tiles(elves, 10)
print(num_empty_tiles)

elves_after_n, i = run_simulation(elves)
print(i)
