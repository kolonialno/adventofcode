from pathlib import Path


def compare(left, right):
    if left == right:
        return None
    elif isinstance(left, int) and isinstance(right, int):
        return left < right
    elif isinstance(left, list) and isinstance(right, list):
        for l, r in zip(left, right):
            comparison = compare(l, r)
            if comparison is not None:
                return comparison
        return None if len(left) == len(right) else len(left) < len(right)
    elif isinstance(left, int):
        return compare([left], right)
    return compare(left, [right])


# Task 1
problem = Path("./input/13.txt").read_text().strip()
pairs = tuple(list(map(eval, pair.split("\n"))) for pair in problem.split("\n\n"))
print(
    sum(
        index + 1
        for index, (first, second) in enumerate(pairs)
        if compare(first, second)
    )
)


# Task 2
class Packet:
    def __init__(self, value):
        self.value = value

    def __lt__(self, other):
        return compare(self.value, other.value)

    def __eq__(self, other):
        return self.value == other.value


packets = [pair[0] for pair in pairs] + [pair[1] for pair in pairs] + [[[2]]] + [[[6]]]
packets = tuple(sorted(map(Packet, packets)))
print((packets.index(Packet([[2]])) + 1) * (packets.index(Packet([[6]])) + 1))
