import numpy as np
import re

def get_board(lines: list):
    board = []
    for j in lines:
        line = []
        for i in re.split('\s+', j):
            try:
                line.append(int(i))
            except:
                pass
        board.append(line)
    board = np.array(board)
    assert board.shape == (5, 5)

    return board

def parse_input(inp: list):
    draws = [int(i) for i in inp[0].split(',')]

    boards = []
    for i in range(2, len(inp), 6):
        boards.append(get_board(inp[i:i+5]))

    boards = np.array(boards)

    return draws, boards

def solve(input_file: str, debug: bool = False):
    with open(input_file) as fh:
        input = fh.readlines()

    draws, boards = parse_input(input)

    print(boards.shape)

    prev_board_win = np.full(boards.shape[0], False)
    for draw in draws:
        mask = boards == draw
        boards[mask] = -1

        mark = boards == -1
        col_win = mark.all(axis=1)
        row_win = mark.all(axis=2)
        board_win = row_win.any(axis=1) | col_win.any(axis=1)

        if board_win.all():
            last_board_win = ~prev_board_win
            assert last_board_win.sum() == 1
            winning_board = boards[last_board_win,...]
            score = (winning_board * (1-mark[last_board_win,...])).sum()
            if debug:
                print(score)
                print(draw)
                print(score * draw)
            break
        else:
            prev_board_win = board_win.copy()

    return score * draw


print(solve("test_input.txt", debug=True))

print(solve("input.txt", debug=True))
