def read_input(filename):
    with open(filename) as f:
        return [int(line) for line in f]


def part1(input):
    return sum(a < b for a, b in zip(input, input[1:]))


def part2(input):
    return part1([a + b + c for a, b, c in zip(input, input[1:], input[2:])])


if __name__ == "__main__":
    input = read_input(__file__[:-3] + ".txt")
    print(f"Part 1: {part1(input)}")
    print(f"Part 2: {part2(input)}")
