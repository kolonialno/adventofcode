valid = 0
required_fields = {
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
}
with open("passports.txt", "r") as fh:

    current_passport_fields = set()
    for line in fh:
        fields = line.split()

        if fields:
            current_passport_fields = current_passport_fields | set([f.split(":")[0] for f in fields])
        else:
            missing_fields = required_fields - current_passport_fields
            if not missing_fields:
                valid += 1
            current_passport_fields = set()

    print(valid)
