import sys
import importlib

def main(day):
    mod = importlib.import_module(f"day{day}.solve")
    with open(f"inputs/{day}/input.txt") as f:
        data = f.read()

    # Part 1
    print("Part 1:", mod.solve1(data))

    # Part 2
    print("Part 2:", mod.solve2(data))

if __name__ == "__main__":
    main(sys.argv[1])