class Assignment:
    def __init__(self, first=None, last=None):
        if first is None or last is None or first > last:
            first, last = None, None
        self.first = first
        self.last = last

    def __eq__(self, other):
        return self.first == other.first and self.last == other.last

    def __le__(self, other):
        if self.first is None:
            return True
        if other.first is None:
            return False
        return self.first >= other.first and self.last <= other.last

    def __bool__(self):
        return self.first is not None

    def __and__(self, other):
        if not (self and other):
            return Assignment()
        return Assignment(max(self.first, other.first), min(self.last, other.last))


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
