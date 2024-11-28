from sys import stdin
import re

lines = [line.split() for line in stdin.read().split("\n\n")]


HCL_REGEX = re.compile(r"^#[0-9a-f]{6}$")
PID_REGEX = re.compile(r"^[0-9]{9}$")


def hgt(x):
    if "cm" in x:
        v = int(x.strip("cm"))
        return 150 <= v <= 193

    if "in" in x:
        v = int(x.strip("in"))
        return 59 <= v <= 76

    return False


field_validators = {
    "byr": lambda x: 1920 <= int(x) <= 2002,
    "iyr": lambda x: 2010 <= int(x) <= 2020,
    "eyr": lambda x: 2020 <= int(x) <= 2030,
    "hgt": hgt,
    "hcl": lambda x: HCL_REGEX.search(x),
    "ecl": lambda x: x in ("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    "pid": lambda x: PID_REGEX.search(x),
    "cid": lambda x: True,
}

expected_fields = field_validators.keys()

passports = [
    dict(item.split(":") for item in passport) for passport in lines  # type: ignore
]


def a():
    total = 0
    for passport in passports:
        keys = passport.keys()

        if keys == expected_fields or keys == expected_fields - {"cid"}:
            total += 1

    return total


def b():
    total = 0
    for passport in passports:
        keys = passport.keys()

        if (keys == expected_fields or keys == expected_fields - {"cid"}) and all(
            field_validators[key](value) for (key, value) in passport.items()
        ):
            total += 1

    return total


print(a())
print(b())