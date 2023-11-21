import sys
import importlib
import time


def main(day):
    day = f"{int(day):02}"
    mod = importlib.import_module(f"solve{day}")
    with open(f"user_inputs/input{day}.txt") as f:
        data = f.read()

    start = time.time()
    result1 = mod.solve1(data)
    end1 = time.time()
    result2 = mod.solve2(data)
    end2 = time.time()
    time1 = (end1 - start) * 1000
    time2 = (end2 - end1) * 1000
    print(f"Part 1 in {time1:.1f} ms:", result1)
    print(f"Part 2 in {time2:.1f} ms:", result2)


if __name__ == "__main__":
    main(sys.argv[1])
