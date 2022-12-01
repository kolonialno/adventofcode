import sys

per_elf = []
total = 0

for line in sys.stdin:
    line = line.strip()
    if line == "":
        per_elf.append(total)
        total = 0
    else:
        total += int(line)

per_elf.append(total) # Last one
per_elf.sort(reverse=True)

print(per_elf[0])

sum = 0
for calories in per_elf[:3]:
    sum += calories
print(sum)
