# Read input
input = open(0).read().strip().split("\n")


# Part 1
total = 0
for y in range(len(input)):
    for x in range(len(input[y])):
        for dx, dy in [(1, 0), (0, 1), (1, 1), (1, -1)]:
            if not x + 3 * dx < len(input[y]) or not 0 <= y + 3 * dy < len(input):
                continue
            word = "".join(input[y + n * dy][x + n * dx] for n in range(4))
            if word in ("XMAS", "SAMX"):
                total += 1

print("Part 1:", total)


# Part 2
total = 0
for y in range(len(input) - 2):
    for x in range(len(input[y]) - 2):
        if (
            input[y + 1][x + 1] == "A"
            and (input[y][x], input[y + 2][x + 2]) in (("M", "S"), ("S", "M"))
            and (input[y + 2][x], input[y][x + 2]) in (("M", "S"), ("S", "M"))
        ):
            total += 1

print("Part 2:", total)
