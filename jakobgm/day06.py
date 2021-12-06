from pathlib import Path

p = Path("inputs/06.txt")
timers = [int(timer) for timer in p.read_text().strip().split(",")]
print(timers)

counts = {
    value: timers.count(value)
    for value
    in range(0, 9)
}

for day in range(1, 257):
    counts = {
        value: counts[value + 1]
        for value
        in range(-1, 8)
    }
    new_timers = counts.pop(-1)
    counts[6] += new_timers
    counts[8] = new_timers

    if day == 80:
        print("Part one: ", sum(counts.values()))

print("Part two: ", sum(counts.values()))
