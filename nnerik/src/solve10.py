def get_delta(data):
    for line in data.splitlines():
        _, *n = line.split()
        yield int(n[0]) if n else 0


def read_signal(data):
    x = 1
    cycle = 0
    for delta in get_delta(data):
        for _ in range(2 if delta else 1):
            yield cycle, x
            cycle += 1
        x += delta


def solve1(data):
    return sum(x * (cycle + 1) for cycle, x in read_signal(data) if cycle % 40 == 19)


def solve2(data):
    result = "\n"
    for cycle, x in read_signal(data):
        result += "#" if x - 1 <= cycle % 40 <= x + 1 else "."
        if cycle % 40 == 39:
            result += "\n"
    return result
