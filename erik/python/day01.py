with open("../input/input01.txt", "r") as input:
    numbers = [int(line) for line in input.readlines()]

total = 2020

# Part 1
print("Part One:")
matches = [n for n in numbers if total - n in numbers]
print(matches, matches[0] * matches[1])

# Part 2
print("Part Two:")
result2 = None
for m in numbers:
    matches = [n for n in numbers if total - m - n in numbers]
    if matches:
        print([m] + matches, m * matches[0] * matches[1])
        break
