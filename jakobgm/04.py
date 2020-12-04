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
        if has_required_fields and not has_additional_fields:
            valid += 1
    print(valid)
