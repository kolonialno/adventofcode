import itertools
from pathlib import Path

# Input parsing
p = Path("inputs/04.txt")
entries = p.read_text().strip().split("\n\n")
draws = list(int(draw) for draw in entries.pop(0).split(","))

boards = []
for entry in entries:
    rows = list(list(map(int, x.split())) for x in entry.strip().split("\n"))
    columns = list(map(list, zip(*rows)))
    boards.append({"rows": rows, "columns": columns})

# How to provide the answer for a given board
# Assuming numbers to have been drawn to be removed from the list(s)
def calculate_answer(board, draw):
    return draw * sum(sum(row) for row in board["rows"])

placement = 1
for draw in draws:
    for board_number, board in enumerate(boards):
        if "placement" in board:
            # We are already done with this board
            continue

        for line in itertools.chain(board["rows"], board["columns"]):
            if draw in line:
                line.remove(draw)
            if not line:
                # The column or row is now empty, and we have a winning board
                boards[board_number]["placement"] = placement
                boards[board_number]["answer"] = calculate_answer(board, draw)
                placement += 1

    if all(("placement" in board) for board in boards):
        break

print(min(boards, key=lambda x: x["placement"])["answer"])
print(max(boards, key=lambda x: x["placement"])["answer"])
