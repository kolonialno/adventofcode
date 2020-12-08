from functools import partial

from voluptuous import Schema, Optional, Coerce, Range, Match, In, All, Length

REQUIRED_FIELDS = ("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")
required_dict = {key: str for key in REQUIRED_FIELDS}
required_dict[Optional("cid")] = str
basic_passport_schema = Schema(required_dict, required=True)


def validate_height(field) -> bool:
    assert len(field) > 2
    value = int(field[:-2])
    unit = field[-2:]
    assert unit in ("cm", "in")
    if unit == "cm" and (150 <= value <= 193):
        return (value, unit)
    if unit == "in" and (59 <= value <= 76):
        return (value, unit)
    raise ValueError


full_passport_schema = Schema(
    {
        "byr": All(Length(4), Coerce(int), Range(1920, 2002)),
        "iyr": All(Length(4), Coerce(int), Range(2010, 2020)),
        "eyr": All(Length(4), Coerce(int), Range(2020, 2030)),
        "hcl": Match("^#[a-f0-9]{6}$"),
        "ecl": In(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
        "pid": Match("^[0-9]{9}$"),
        "hgt": validate_height,
        Optional("cid"): str,
    },
    required=True,
)


def extract_key_values(chunk):
    return dict((x.split(":")) for x in chunk.split(" "))


def parse_passport_file(filename) -> list:
    with open(filename) as in_file:
        data = in_file.read()
    passport_chunks = (x.replace("\n", " ") for x in data.split("\n\n"))
    passport_data = (extract_key_values(x) for x in passport_chunks)
    return passport_data


def has_all_fields(required_fields, passport):
    return all(passport.get(field) for field in required_fields)


def is_valid_voluptuous(schema, passport):
    try:
        schema(passport)
        return True
    except Exception:
        return False


def testcase_basic() -> int:
    entries = parse_passport_file("inputs/04testcase.txt")
    return len(list(filter(partial(has_all_fields, REQUIRED_FIELDS), entries)))


def testcase() -> int:
    entries = parse_passport_file("inputs/04testcase.txt")
    return len(
        list(filter(partial(is_valid_voluptuous, basic_passport_schema), entries))
    )


def main() -> int:
    entries = parse_passport_file("inputs/04.txt")
    return len(
        list(filter(partial(is_valid_voluptuous, basic_passport_schema), entries))
    )


def secondary() -> int:
    entries = parse_passport_file("inputs/04.txt")
    return len(
        list(filter(partial(is_valid_voluptuous, full_passport_schema), entries))
    )


def testcase_secondary_valid() -> int:
    entries = parse_passport_file("inputs/04testcase_valid.txt")
    return len(
        list(filter(partial(is_valid_voluptuous, full_passport_schema), entries))
    )


def testcase_secondary_invalid() -> int:
    entries = parse_passport_file("inputs/04testcase_invalid.txt")
    return len(
        list(filter(partial(is_valid_voluptuous, full_passport_schema), entries))
    )
