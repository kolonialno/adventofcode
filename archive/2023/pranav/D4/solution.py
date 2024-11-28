def lines():
    with open("input.txt") as f:
        return f.readlines()


def winning_numbers(part):
    ps = part.split(":")[1].split(" ")
    return {p.strip() for p in ps if p}


def my_numbers(part):
    ps = part.split(" ")
    return {p.strip() for p in ps if p}


def part1():
    total = 0
    for line in lines():
        parts = line.split("|")
        w_nums = winning_numbers(parts[0])
        m_nums = my_numbers(parts[1])

        if same := len(m_nums.intersection(w_nums)):
            total += 2 ** (same - 1)

    print(total)


def part2():
    total = 0

    ls = lines()
    copies = [0] * len(ls)

    for i, line in enumerate(ls):
        parts = line.split("|")
        w_nums = winning_numbers(parts[0])
        m_nums = my_numbers(parts[1])

        if w_count := len(m_nums.intersection(w_nums)):
            start = i + 1
            end = start + w_count

            # Add the copies.
            for y in range(start, end):
                copies[y] += copies[i] + 1

        copies[i] += 1  # Original

    total = sum(copies)
    print(total)


part2()
