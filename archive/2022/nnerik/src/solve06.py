def solve(data, size):
    for i, _ in enumerate(data):
        if len(set(data[i : i + size])) == size:
            return i + size


def solve1(data):
    return solve(data, 4)


def solve2(data):
    return solve(data, 14)
