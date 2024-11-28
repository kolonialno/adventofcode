import re


def lines():
    with open("input.txt") as f:
        return f.readlines()


def find_all_numbers(line):
    p = r"(\d+)"
    return [(int(m.group()), m.start(), m.end() - 1) for m in re.finditer(p, line)]


def find_all_gears(line):
    p = r"(\*)"
    return [m.start() for m in re.finditer(p, line)]


def is_symbol(c):
    print("c:", c)
    return c.strip() not in [".", "\n"] and not c.isdigit()


def found_around(lines, row, start, end):
    if start > 0 and is_symbol(lines[row][start - 1]):
        return True

    word_length = len(lines[0].strip()) - 1

    if end < word_length and is_symbol(lines[row][end + 1]):
        return True

    s = 0 if start == 0 else start - 1
    e = word_length if end == word_length else end + 1

    if row > 0:
        for r in range(s, e + 1):
            if is_symbol(lines[row - 1][r]):
                return True

    if row < len(lines) - 1:
        for r in range(s, e + 1):
            if is_symbol(lines[row + 1][r]):
                return True

    return False


def numbers_around_gear(lines, numbers, row, col):
    parts = []
    for num in numbers:
        match num:
            case (n, _, e, r) if e == col - 1 and row == r:
                parts.append(n)
            case (n, s, _, r) if s == col + 1 and row == r:
                parts.append(n)
            case (n, s, e, r) if (s <= col + 1 and e >= col - 1) and (
                row - 1 == r or row + 1 == r
            ):
                parts.append(n)
            case _:
                pass

    if parts and len(parts) == 2:
        return parts
    return None, None


def part1():
    numbers = []
    ls = lines()
    for i, l in enumerate(ls):
        ns = find_all_numbers(l.strip())
        numbers += [n[0] for n in ns if found_around(ls, i, n[1], n[2])]

    print(sum(numbers))


def part2():
    numbers = []
    ls = lines()
    gears = []
    for i, l in enumerate(ls):
        ns = find_all_numbers(l.strip())
        gears += [(i, c) for c in find_all_gears(l.strip())]
        numbers += [(n[0], n[1], n[2], i) for n in ns]

    result = 0
    for row, col in gears:
        a, b = numbers_around_gear(lines, numbers, row, col)
        if a and b:
            result += a * b

    print(result)


part2()
