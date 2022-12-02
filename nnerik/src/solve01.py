def solve1(data):
    return max(sum(map(int, calories.split())) for calories in data.split("\n\n"))


def solve2(data):
    return sum(
        sorted(sum(map(int, calories.split())) for calories in data.split("\n\n"))[-3:]
    )
