import itertools
from pathlib import Path

INPUT_DIR = Path(__file__).parent / "input"


if __name__ == "__main__":
    input_path = INPUT_DIR / "day02a.txt"
    rows = input_path.read_text().strip().split("\n")
    valid = 0
    for row in rows:
        policy, password = row.split(":")
        limits, letter = policy.split(" ")
        lower, upper = limits.split("-")
        lower, upper = int(lower), int(upper)
        if password.count(letter) in range(lower, upper + 1):
            valid += 1
    print(valid)
