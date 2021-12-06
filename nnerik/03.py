from functools import reduce


def read_input(filename):
    with open(filename) as f:
        return [[int(c) for c in line.strip()] for line in f]


def condense(input):
    balance = [0 for _ in range(len(input[0]))]
    for line in input:
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


if __name__ == "__main__":
    input = read_input(__file__[:-3] + ".txt")

    # Part 1
    gamma, epsilon = condense(input)
    result = bits_to_int(gamma) * bits_to_int(epsilon)
    print(f"Part 1: {result}")

    # Part 2
    oxy = get_rating(input, lambda lst: condense(lst)[0])
    co2 = get_rating(input, lambda lst: condense(lst)[1])
    print(f"Part 2: {oxy * co2}")
