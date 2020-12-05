from pathlib import Path

INPUT_DIR = Path(__file__).parent / "input"

input_path = INPUT_DIR / "day05.txt"
problem = input_path.read_text().strip()
seat_ids = []
for boarding_pass in problem.split("\n"):
    lower, upper = 0, 127
    for letter in boarding_pass[:7]:
        elimination = (upper - lower + 1) // 2
        if letter == "F":
            upper = upper - elimination
        else:
            lower = lower + elimination
    assert lower == upper
    row = lower

    lower, upper = 0, 7
    for letter in boarding_pass[7:]:
        elimination = (upper - lower + 1) // 2
        if letter == "L":
            upper = upper - elimination
        else:
            lower = lower + elimination
    assert lower == upper
    column = lower

    seat_id = 8 * row + column
    seat_ids.append(seat_id)

print(max(seat_ids))
