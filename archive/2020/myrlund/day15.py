test_input = [0, 3, 6]
puzzle_input = [2, 0, 6, 12, 1, 3]

inp = puzzle_input
target_rnd = 30_000_000

rnd_last_spoken = [None] * target_rnd
for rnd0, n in enumerate(inp):
    rnd_last_spoken[n] = rnd0 + 1

next_number = 0

for rnd in range(len(inp) + 1, target_rnd):
    number = next_number
    last_spoken = rnd_last_spoken[number]
    rnd_last_spoken[number] = rnd
    next_number = rnd - last_spoken if last_spoken is not None else 0

print(next_number)
