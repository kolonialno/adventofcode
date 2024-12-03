import re

# Read input
input = open(0).read()


# Part 1
instructions = re.finditer(r"mul\((\d{1,3}),(\d{1,3})\)", input)
total = 0
for pair in instructions:
    a, b = pair.groups()
    total += int(a) * int(b)

print("Part 1:", total)


# Part 2
total = 0
instructions = re.finditer(r"(do)\(\)|(don't)\(\)|mul\((\d{1,3}),(\d{1,3})\)", input)

do = True
for instruction in instructions:
    if instruction.group(1) == "do":
        do = True
        continue
    elif instruction.group(2) == "don't":
        do = False
        continue
    elif do:
        a, b = instruction.group(3, 4)
        total += int(a) * int(b)

print("Part 2:", total)
