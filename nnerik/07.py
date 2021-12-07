def read_input(filename):
    with open(filename) as f:
        return [int(n) for n in f.readline().split(",")]


def solve(crabs, cost):
    return min(
        sum(cost(abs(c - i)) for c in crabs) for i in range(min(crabs), max(crabs) + 1)
    )


if __name__ == "__main__":
    input = read_input(__file__[:-3] + ".txt")

    # Part 1
    result = solve(input, lambda a: a)
    print(f"Part 1: {result}")

    # Part 2
    result = solve(input, lambda a: a * (a + 1) // 2)
    print(f"Part 2: {result}")
