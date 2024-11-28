import re
from pathlib import Path

INPUT_DIR = Path(__file__).parent / "input"

if __name__ == "__main__":
    input_path = INPUT_DIR / "day04.txt"
    required_fields = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
    optional_fields = {"cid"}
    allowed_fields = required_fields | optional_fields
    valid = 0
    for passport in input_path.read_text().strip().split("\n\n"):
        items = passport.replace("\n", " ").split(" ")
        passport = dict(item.split(":") for item in items)
        fields = set(passport.keys())

        has_required_fields = len(required_fields.intersection(fields)) == len(
            required_fields
        )
        has_additional_fields = len(fields - allowed_fields) != 0
        if not has_required_fields or has_additional_fields:
            continue

        if not (1920 <= int(passport["byr"]) <= 2002):
            continue
        if not (2010 <= int(passport["iyr"]) <= 2020):
            continue
        if not (2020 <= int(passport["eyr"]) <= 2030):
            continue

        height = passport["hgt"]
        height, units = int(height[:-2]), height[-2:]
        if units not in {"cm", "in"}:
            continue
        ranges = {"cm": range(150, 193 + 1), "in": range(59, 76 + 1)}
        if height not in ranges[units]:
            continue

        if not re.match(r"^#[0-9a-f]{6}$", passport["hcl"]):
            continue
        if passport["ecl"] not in {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}:
            continue
        if not re.match(r"^[0-9]{9}$", passport["pid"]):
            continue
        valid += 1

    print(valid)
