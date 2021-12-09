digits = "abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg"
fingerprint = lambda pattern, digit: sum(
    sorted({c: pattern.count(c) for c in "abcdefg"}[segment] for segment in digit)
)
key = {fingerprint(digits, digit): str(n) for n, digit in enumerate(digits.split())}
decode = lambda pattern, output: "".join(
    key[fingerprint(pattern, digit)] for digit in output.split()
)
results = [decode(*line.split("|")) for line in open("input.txt").readlines()]

print("Part 1:", sum(1 for output in results for d in output if d in "1478"))
print("Part 2:", sum(int(n) for n in results))
