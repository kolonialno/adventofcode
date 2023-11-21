from pathlib import Path
import numpy as np

data = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
""".split("\n")

data = Path("day01/input").read_text().splitlines()

elves = []

elf = []
for row in data:
    if row == "":
        elves.append(sum(elf))
        elf = []
    else:
        elf.append(int(row))
elves.append(sum(elf))

print(max(elves))
print(sum(sorted(elves)[::-1][:3]))