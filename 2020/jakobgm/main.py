import itertools
from pathlib import Path

INPUT_DIR = Path(__file__).parent / "input"


if __name__ == "__main__":
    input_path = INPUT_DIR / "day01a.txt"
    numbers = {int(number) for number in input_path.read_text().split("\n") if number}
    for number in numbers:
        complement = 2020 - number
        if complement in numbers:
            print(f"01a: {number * complement}")
            break

    combinations = itertools.combinations(numbers, 3)
    for combination in combinations:
        if combination[0] + combination[1] + combination[2] == 2020:
            print("01b:", combination[0] * combination[1] * combination[2])
