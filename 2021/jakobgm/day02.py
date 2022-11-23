from pathlib import Path

input_path = Path("inputs/02.txt")
instructions = tuple(
    instruction.split() for instruction in input_path.read_text().strip().split("\n")
)

horizontal, depth = 0, 0
for direction, step in instructions:
    if direction == "forward":
        horizontal += int(step)
    elif direction == "down":
        depth += int(step)
    elif direction == "up":
        depth -= int(step)
    else:
        raise RuntimeError
print(horizontal * depth)

horizontal, depth, aim = 0, 0, 0
for direction, step in instructions:
    if direction == "up":
        aim -= int(step)
    elif direction == "down":
        aim += int(step)
    elif direction == "forward":
        horizontal += int(step)
        depth += int(step) * aim
    else:
        raise RuntimeError
print(horizontal * depth)
