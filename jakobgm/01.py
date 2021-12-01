from pathlib import Path

p = Path("input.txt")
measures = p.read_text().strip().split("\n")
measures = [int(x) for x in measures]

increasing = 0
for previous, this in zip(measures[:-1], measures[1:]):
    if previous < this:
        increasing += 1
print(increasing)

windows = []
for a, b, c in zip(measures[:-2], measures[1:-1], measures[2:]):
    windows.append(a + b + c)

increasing = 0
for previous, this in zip(windows[:-1], windows[1:]):
    if previous < this:
        increasing += 1

print(increasing)
