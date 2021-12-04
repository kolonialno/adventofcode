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

def calculate_answer(board, draw):
    return draw * sum(sum(row) for row in board["rows"])

found = False
placement = 1
for draw in draws:
    for board_number, board in enumerate(boards):
        if "placement" in board:
            continue
        for column in board['columns']:
            if draw in column:
                column.remove(draw)
            if not column:
                boards[board_number]["placement"] = placement
                placement += 1
                boards[board_number]["answer"] = calculate_answer(board, draw)
                break
        for row in board['rows']:
            if draw in row:
                row.remove(draw)
            if not row:
                boards[board_number]["placement"] = placement
                placement += 1
                boards[board_number]["answer"] = calculate_answer(board, draw)
                break
    if all(("placement" in board) for board in boards):
        break

print(min(boards, key=lambda x: x["placement"])["answer"])
print(max(boards, key=lambda x: x["placement"])["answer"])
