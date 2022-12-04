class Assignment(set):
    def __init__(self, first=None, last=None):
        if first is None or last is None or first > last:
            super().__init__()
            return
        super().__init__(range(first, last + 1))


def parse_assignment_pair(spec):
    ranges = spec.split(",")
    return tuple(Assignment(*map(int, r.split("-"))) for r in ranges)


def solve1(data):
    return sum(
        a <= b or b <= a
        for a, b in (parse_assignment_pair(line) for line in data.splitlines())
    )


def solve2(data):
    return sum(
        bool(a & b)
        for a, b in (parse_assignment_pair(line) for line in data.splitlines())
    )
