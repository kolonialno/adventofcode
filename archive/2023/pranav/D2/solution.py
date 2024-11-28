import re


def lines():
    with open("input.txt") as f:
        return f.readlines()


def enough(line, color, count):
    p = rf"(\d+)\s(?={color})"  # Lesson from yesterday lol
    found = re.findall(p, line)
    return sum(map(lambda x: int(x), found)) <= count


def cubes(line, color):
    p = rf"(\d+)\s(?={color})"
    found = re.findall(p, line)
    return sum(map(lambda x: int(x), found))


def part1():
    total = 0
    for i, l in enumerate(lines()):
        ok = True
        for s in l.split(";"):
            if (
                not enough(s, "green", 13)
                or not enough(s, "blue", 14)
                or not enough(s, "red", 12)
            ):
                ok = False
        if ok:
            total += i + 1
    print(total)


def part2():
    total = 0
    for l in lines():
        green = 0
        red = 0
        blue = 0

        for s in l.split(";"):
            green = max(green, cubes(s, "green"))
            red = max(red, cubes(s, "red"))
            blue = max(blue, cubes(s, "blue"))

        total += green * red * blue

    print(total)


part1()
part2()
