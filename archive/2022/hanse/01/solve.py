from sys import stdin

elfs = [0]
for line in stdin:
    line = line.strip()
    if not line:
        elfs.append(0)
    else:
        elfs[-1] += int(line)

print(max(elfs))
print(sum(sorted(elfs, reverse=True)[:3]))
