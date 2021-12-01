from pathlib import Path

INPUT_DIR = Path(__file__).parent / "input"


def read_input(filename):
    with open(INPUT_DIR / filename) as f:
        return [int(line) for line in f.readlines()]

def solve_part1(input):
    return sum(a < b for a, b in zip(input, input[1:]))

def solve_part2(input):
    series = [a + b + c for a, b, c in zip(input, input[1:], input[2:])]
    return sum(a < b for a, b in zip(series, series[1:]))

if __name__ == "__main__":
    input = read_input("day01.txt")
    print(f"Part 1: {solve_part1(input)}")
    print(f"Part 2: {solve_part2(input)}")
