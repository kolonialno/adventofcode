from typing import List, Tuple

import numpy as np


def read_input(filename) -> Tuple[np.ndarray, List[np.ndarray]]:
    with open(filename) as f:
        draws = np.array(f.readline().rstrip().split(",")).astype(int)
        boards = [
            np.array(
                [[n for n in line.strip().split()] for line in board.split("\n")]
            ).astype(int)
            for board in f.read().strip().split("\n\n")
        ]
        return draws, boards


def run_bingo(draws: np.ndarray, boards: List[np.ndarray]) -> List[Tuple[int, int]]:
    winner_sequence: List[Tuple[int, int]] = []  # (index, score)
    board_indexes = set(range(len(boards)))

    for draw_index, _ in enumerate(draws):
        draws_so_far = draws[:draw_index]

        winners = set([i[0] for i in winner_sequence])
        remaining_boards = winners ^ board_indexes

        for board_index in remaining_boards:
            board = boards[board_index]
            hit_matrix = np.isin(board, draws_so_far)
            if hit_matrix.all(axis=0).any() or hit_matrix.all(axis=1).any():
                # Bingo!
                score = board[~hit_matrix].sum() * draws_so_far[-1]
                winner_sequence.append((board_index, score))

    return winner_sequence


bingo_session_result = run_bingo(*read_input("inputs/04.txt"))

answer1 = bingo_session_result[0][1]  # First winner score
answer2 = bingo_session_result[-1][1]  # Last winner score

print(f"Answer1: {answer1}")  # 45031
print(f"Answer2: {answer2}")  # 2568
