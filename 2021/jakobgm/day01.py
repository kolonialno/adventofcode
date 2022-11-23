from pathlib import Path

# Input parsing
p = Path("inputs/01.txt")
measures = tuple(int(digit) for digit in p.read_text().strip().split("\n"))

# Task a
increasing = 0
for previous, current in zip(measures[:-1], measures[1:]):
    if previous < current:
        increasing += 1
print(increasing)

# Task b
windows = tuple(
    a + b + c for a, b, c in zip(measures[:-2], measures[1:-1], measures[2:])
)
increasing_windows = 0
for previous, this in zip(windows[:-1], windows[1:]):
    if previous < this:
        increasing_windows += 1
print(increasing_windows)
