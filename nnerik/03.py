from functools import reduce


def read_input(filename):
    with open(filename) as f:
        return [[int(c) for c in reversed(line.strip())] for line in f.readlines()]


def get_most_common_bits(input):
    counts = [0 for _ in range(len(input[0]))]
    for line in input:
        for i, bit in enumerate(line):
            counts[i] += 1 if bit else -1

    return [1 if c >= 0 else 0 for c in counts]


def bits_to_int(bits):
    return reduce(lambda n, b: n * 2 + b, reversed(bits))


def get_rating(input, criteria_func):
    for i in reversed(range(len(input[0]))):
        if len(input) == 1:
            return bits_to_int(input[0])
        pattern = criteria_func(input)
        input = list(filter(lambda line: line[i] == pattern[i], input))


def part1(input):
    gamma_bits = get_most_common_bits(input)

    gamma = bits_to_int(gamma_bits)
    epsilon = bits_to_int([1 - b for b in gamma_bits])

    return gamma * epsilon


def part2(input):
    oxy_rating = get_rating(input, get_most_common_bits)

    get_least_common_bits = lambda lst: [1 - b for b in get_most_common_bits(lst)]
    co2_rating = get_rating(input, get_least_common_bits)

    return oxy_rating * co2_rating


if __name__ == "__main__":
    input = read_input(__file__[:-3] + ".txt")
    print(f"Part 1: {part1(input)}")
    print(f"Part 2: {part2(input)}")
