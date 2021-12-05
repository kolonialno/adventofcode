from collections import defaultdict
from sys import stdin

draws = [int(n) for n in next(stdin).split(",")]
next(stdin)

boards = defaultdict(list)

board_id = 1
i = 0
for line in stdin:
    value = line.strip()
    if not value:
        continue
    i += 1
    boards[board_id].append([int(n) for n in value.split()])
    if i == 5:
        i = 0
        board_id += 1


def play(winner_index):
    combos = []
    for board, rows in boards.items():
        for line in rows:
            combos.append((board, set(line), line))

        for x in range(5):
            line = [rows[y][x] for y in range(5)]
            combos.append((board, set(line), line))

    def winner():
        won = {}
        winners = []
        for draw in draws:
            for board, combo, original in combos:
                if board in won:
                    continue
                combo.discard(draw)
                if len(combo) == 0:
                    won[board] = board, draw, original
                    winners.append(board)

        return won[winners[winner_index]]

    board, draw, winning = winner()

    unmarked = set()
    for combo in combos:
        if combo[0] == board:
            unmarked = unmarked | combo[1]

    sum_unmarked = sum(unmarked - set(winning))

    return sum_unmarked * draw


def a():
    return play(0)


def b():
    return play(-1)


print(a())
print(b())
