from pathlib import Path
import numpy as np

test = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7""".strip().split("\n")
test1_result = 4512
test2_result = 2


txt = Path("inputs/04.txt").read_text().strip().split("\n")

def read(txt):
    numbers = txt[0].strip().split(",")
    rest = txt[1:]
    i = 0
    boards = []
    while True:
        start = 6*i+1
        finish = 6*i+6
        if start > len(rest):
            break
        board = rest[start:finish]
        i += 1
        boards.append(board)
    new_boards = []
    for board in boards:
        new_board = []
        for line in board:
            line = line.strip().split()
            new_board.append(line)
        new_boards.append(np.array(new_board, dtype=int))
    new_boards = new_boards
    numbers = np.array(numbers, dtype=int)
    return numbers, new_boards

def five_in_a_row(X):
    vertical = (X.sum(0) == 5)
    horizontal = (X.sum(1) == 5)
    return vertical.any() or horizontal.any()

def loop_boards(numbers, boards, part="a"):
    X_boards = [np.zeros_like(b, dtype=bool) for b in boards]
    for number in numbers:
        for i, (board, X) in enumerate(zip(boards, X_boards)):
            mask = board == number
            X[mask] = True
            five = five_in_a_row(X)
            if five:
                if part == 'a':
                    print(f"PART A:{board[~X].sum() * number}")
                    return board[~X].sum() * number
                else:
                    print(board[~X].sum() * number)
                    boards.pop(i)
                    return boards

def func1(txt):
    numbers, boards = read(txt)
    return loop_boards(numbers, boards, part="a")
    

def test1():
    result = func1(test)
    assert result == test1_result
    print("TEST 1 PASS")

def func2(txt):
    numbers, boards = read(txt)
    while boards:
        boards = loop_boards(numbers, boards, part="b")

test1()
print(func1(txt))
print(func2(txt))