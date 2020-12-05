def parse(ranges, code):
    for half in code:
        ranges[half] = ranges[0] + (ranges[1] - ranges[0]) // 2
    return ranges[0]

with open("passes.txt", "r") as fh:
    for line in fh.readlines():
        row = parse([0, 128], (1 if c == "F" else 0 for c in line[:7]))
        column = parse([0, 8], (1 if c == "L" else 0 for c in line[7:]))
        seat_ids.append((row * 8) + column)

    seat_ids = sorted(seat_ids)
    print("Higest ID", max(seat_ids))

    for idx in range(len(seat_ids) - 1):
        current_id = seat_ids[idx]
        if seat_ids[idx + 1] != current_id + 1:
            print("Missing ID/Your ID", current_id + 1)
