import re
hair_re = re.compile("^#[0-9a-f]{6}$")
pid_re = re.compile("^\d{9}$")

with open("passports.txt", "r") as fh:
    valid = 0
    current_passport_fields = {}
    for line in fh:
        fields = line.split()

        if fields:
            for field in fields:
                key, value = field.split(":", 2)
                current_passport_fields[key] = value

        else:

            missing_fields = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"} - set(current_passport_fields.keys())
            required_fields_present = bool(not missing_fields)

            if required_fields_present:
                for key, value in current_passport_fields.items():

                    if key == "byr":
                        if not (int(value) >= 1920 and int(value) <= 2002):
                            break
                    elif key == "iyr":
                        if not (int(value) >= 2010 and int(value) <= 2020):
                            break
                    elif key == "eyr":
                        if not (int(value) >= 2020 and int(value) <= 2030):
                            break
                    elif key == "hgt":
                        if not value[-2:] in {"cm", "in"}:
                            break
                        if (value.endswith("cm") and not (int(value[:-2]) >= 150 and int(value[:-2]) <= 193)):
                            break
                        elif (value.endswith("in") and not (int(value[:-2]) >= 59 and int(value[:-2]) <= 76)):
                            break
                    elif key == "hcl":
                        if not re.fullmatch(hair_re, value):
                            break
                    elif key == "ecl":
                        if not (value in {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}):
                            break
                    elif key == "pid":
                        if not re.fullmatch(pid_re, value):
                            break

                else:
                    valid += 1

            # Reset
            current_passport_fields = {}

    print(valid)
