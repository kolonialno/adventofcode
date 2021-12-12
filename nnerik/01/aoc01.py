def count_runs(seq):
    return sum(a < b for a, b in zip(seq, seq[1:]))


with open("input.txt") as f:
    seq = [int(line) for line in f]

print("Part 1:", count_runs(seq))
print("Part 2:", count_runs([a + b + c for a, b, c in zip(seq, seq[1:], seq[2:])]))
