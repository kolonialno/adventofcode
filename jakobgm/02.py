from pathlib import Path

INPUT_DIR = Path(__file__).parent / "input"


if __name__ == "__main__":
    input_path = INPUT_DIR / "day02a.txt"
    rows = input_path.read_text().strip().split("\n")
    valid = 0
    valid_new_policy = 0
    for row in rows:
        policy, password = row.split(": ")
        limits, letter = policy.split(" ")
        lower, upper = limits.split("-")
        lower, upper = int(lower), int(upper)

        # Old password policy
        if password.count(letter) in range(lower, upper + 1):
            valid += 1

        # New password policy
        first_match = password[lower - 1] == letter
        second_match = password[upper - 1] == letter
        if (first_match or second_match) and (first_match != second_match):
            valid_new_policy += 1

    print("02a:", valid)
    print("02b:", valid_new_policy)
