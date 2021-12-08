with open("input.txt") as f:
    result = sum(
        sum(len(digit) in {2, 3, 4, 7} for digit in output.split())
        for output in (line.split("|")[1] for line in f)
    )
    print("Part 1:", result)

digits = "abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg"
code = lambda pattern, digit: "".join(
    sorted(str({c: pattern.count(c) for c in "abcdefg"}[segment]) for segment in digit)
)
key = {code(digits, digit): str(n) for n, digit in enumerate(digits.split())}
decode = lambda pattern, output: int(
    "".join(key[code(pattern, digit)] for digit in output.split())
)
with open("input.txt") as f:
    print("Part 2:", sum(decode(*line.split("|")) for line in f))
