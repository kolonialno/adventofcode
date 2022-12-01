from collections import defaultdict

i = 1
elves = defaultdict(lambda: 0)

with open("input.txt", "r") as f:
    while line := f.readline():
        if line != '\n':
            elves[i] += int(line)
        else:
            i +=1

print(max(elves.values()))
print(sum(sorted(elves.values())[-3:]))
