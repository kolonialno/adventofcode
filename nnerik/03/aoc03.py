from functools import reduce


def condense(report):
    balance = [0 for _ in range(len(report[0]))]
    for line in report:
        for i, bit in enumerate(line):
            balance[i] += 1 if bit else -1

    return [0 if c < 0 else 1 for c in balance], [1 if c < 0 else 0 for c in balance]


def bits_to_int(bits):
    return reduce(lambda n, b: n * 2 + b, bits)


def get_rating(input, criteria_func):
    for i in range(len(input[0])):
        if len(input) == 1:
            return bits_to_int(input[0])
        pattern = criteria_func(input)
        input = [line for line in input if line[i] == pattern[i]]


with open("input.txt") as f:
    report = [[int(c) for c in line.strip()] for line in f]

gamma, epsilon = condense(report)
result = bits_to_int(gamma) * bits_to_int(epsilon)
print("Part 1:", result)

oxy = get_rating(report, lambda lst: condense(lst)[0])
co2 = get_rating(report, lambda lst: condense(lst)[1])
print("Part 2:", oxy * co2)
