def fingerprint(pattern, digit):
    return sum({c: pattern.count(c) for c in "abcdefg"}[segment] for segment in digit)


def decode(key, pattern, output):
    return "".join(key[fingerprint(pattern, digit)] for digit in output.split())


digits = "abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg"
prints = {fingerprint(digits, digit): str(n) for n, digit in enumerate(digits.split())}

with open("input.txt") as f:
    results = [decode(prints, *line.split("|")) for line in f]

print("Part 1:", sum(1 for output in results for d in output if d in "1478"))
print("Part 2:", sum(int(n) for n in results))
