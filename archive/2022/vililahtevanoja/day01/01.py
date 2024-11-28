data = []
with open("input.txt") as f:
    data = [l.strip() for l in f.readlines()]

grouped_data = []
acc_data = []
for line in data:
    if line == "":
        grouped_data.append(acc_data)
        acc_data = []
        continue
    acc_data.append(int(line))
sums = [sum(ns) for ns in grouped_data]


print(f"Part 1: {max(sums)}")
print(f"Part 2: {sum(sorted(sums, reverse=True)[:3])}")