import re


def lines():
    with open("input.txt") as f:
        return f.readlines()


def part1():
    total = 0
    for l in lines():
        factor1 = re.search("\d", l).group()
        factor2 = re.search("\d", l[::-1]).group()
        total += int(f"{factor1}{factor2}")
    print(total)


def part2():
    # meh.
    total = 0
    numbers = {
        "one": 1,
        "two": 2,
        "three": 3,
        "four": 4,
        "five": 5,
        "six": 6,
        "seven": 7,
        "eight": 8,
        "nine": 9,
    }
    for l in lines():
        digits = re.findall("(?=(\d|one|two|three|four|five|six|seven|eight|nine))", l)
        f1 = digits[0]
        f2 = digits[-1]
        total += int(f"{numbers.get(f1, f1)}{numbers.get(f2, f2)}")
    print(total)


part2()
