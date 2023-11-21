directions = {"R": (1, 0), "L": (-1, 0), "U": (0, 1), "D": (0, -1)}


def move_head(pos, direction):
    return (pos[0] + directions[direction][0], pos[1] + directions[direction][1])


def move_tail(new_head_pos, tail_pos):

    move_x = 0
    move_y = 0
    x_diff = new_head_pos[0] - tail_pos[0]
    y_diff = new_head_pos[1] - tail_pos[1]

    if abs(x_diff) > 1 or abs(y_diff) > 1:

        if y_diff != 0:
            move_y = y_diff // abs(y_diff)

        if x_diff != 0:
            move_x = x_diff // abs(x_diff)

    return (tail_pos[0] + move_x, tail_pos[1] + move_y)


def solve(moves, num_knots):

    new_posistion = [(0, 0)] * num_knots
    position = [(0, 0)] * num_knots

    tail_positions = set()

    for direction, steps in moves:
        for _ in range(1, steps + 1):

            new_posistion[0] = move_head(position[0], direction)

            for k in range(1, num_knots):
                new_posistion[k] = move_tail(new_posistion[k - 1], position[k])

            position = new_posistion.copy()
            tail_positions.add(position[num_knots - 1])

    print(len(tail_positions))


moves = []
with open("input.txt") as f:
    for l in f.readlines():
        direction, steps = l.split()
        moves.append((direction, int(steps)))


solve(moves, 2)
solve(moves, 10)
