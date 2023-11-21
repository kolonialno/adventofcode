import enum
from pathlib import Path

problem = Path("./input/10.txt").read_text().strip().splitlines()

X = 1
cycle = 1
history = [X]
for instruction in problem:
    history.append(X)
    cycle += 1
    if instruction != "noop":
        history.append(X)
        cycle += 1
        X += int(instruction.split()[1])

print(sum(cycle * history[cycle] for cycle in range(20, 221, 40)))
for cycle in range(1, len(history)):
    x_position = (cycle - 1) % 40
    if abs(history[cycle] - x_position) <= 1:
        print("#", end="\n" if x_position == 39 else "")
    else:
        print(".", end="\n" if x_position == 39 else "")
