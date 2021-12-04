from typing import List, Tuple

import pandas as pd

# Read and prepare data
file = open("inputs/04.txt")
draws = pd.Series(file.readline().rstrip().split(",")).astype(int)
board_data = pd.read_csv(file, sep=r"\s+", header=None, skip_blank_lines=True)

boards: List[pd.DataFrame] = []
board_dimension = 5

# Slice each board into separate frames
for start_index in range(0, len(board_data), board_dimension):
    boards.append(board_data.iloc[start_index : start_index + board_dimension, :])


def run_bingo(draws: pd.Series, boards: List[pd.DataFrame]) -> List[Tuple[int, int]]:
    winner_sequence: List[Tuple[int, int]] = []  # (index, score)
    board_indexes = set(range(len(boards)))

    # No point in checking for bingo too early
    for draw_index in draws.index[len(boards[0]) :]:
        draws_so_far = draws[:draw_index]

        winners = set([i[0] for i in winner_sequence])
        remaining_boards = winners ^ board_indexes

        for board_index in remaining_boards:
            board = boards[board_index]
            for i in range(len(board)):
                if (
                    board.iloc[i].isin(draws_so_far).all()
                    or board.T.iloc[i].isin(draws_so_far).all()
                ):
                    # Bingo!
                    board_stack = board.stack()
                    score = (
                        board_stack[~board_stack.isin(draws_so_far)].sum()
                        * draws_so_far.iloc[-1]
                    )
                    winner_sequence.append((board_index, score))
                    break
    return winner_sequence


bingo_session_result = run_bingo(draws, boards)

answer1 = bingo_session_result[0][1]  # First winner score
answer2 = bingo_session_result[-1][1]  # Last winner score

print(f"Answer1: {answer1}")  # 45031
print(f"Answer2: {answer2}")  # 2568
