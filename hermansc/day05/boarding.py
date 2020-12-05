import math

with open("passes.txt", "r") as fh:
    seat_ids = []
    for line in fh:
        row, column = None, None
        rows = (0, 127)
        columns = (0, 7)
        for char in line.strip():
            if char == "F":
                rows = (rows[0], rows[0] + (rows[1] - rows[0])//2)
            elif char == "B":
                rows = (rows[0] + math.ceil((rows[1] - rows[0])/2), rows[1])
            elif char == "R":
                columns = (columns[0] + math.ceil((columns[1] - columns[0])/2), columns[1])
            elif char == "L":
                columns = (columns[0], columns[0] + (columns[1] - columns[0])//2)

            if rows[0] == rows[1]:
                row = rows[0]
            if columns[0] == columns[1]:
                column = columns[0]

        seat_ids.append((row * 8) + column)

    seat_ids = sorted(seat_ids)
    print("Higest ID", max(seat_ids))

    for idx in range(len(seat_ids) - 1):
        current_id = seat_ids[idx]
        if seat_ids[idx + 1] != current_id + 1:
            print("Missing ID/Your ID", current_id + 1)
