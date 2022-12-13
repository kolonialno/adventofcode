import json

from functools import total_ordering


@total_ordering
class Packet:
    def __init__(self, input):
        input = json.loads(input) if isinstance(input, str) else input
        if isinstance(input, int):
            self.value = input
        elif isinstance(input, list):
            self.value = tuple(Packet(i) for i in input)

    def __eq__(self, other):
        if type(self.value) == type(other.value):
            return self.value == other.value
        if isinstance(self.value, int):
            return (Packet(self.value),) == other.value
        return self.value == (Packet(other.value),)

    def __le__(self, other):
        if type(self.value) == type(other.value):
            return self.value <= other.value
        if isinstance(self.value, int):
            return (Packet(self.value),) <= other.value
        return self.value <= (Packet(other.value),)


def get_pairs(data):
    for pair in data.split("\n\n"):
        left, right = pair.splitlines()
        yield Packet(left), Packet(right)


def get_packets(data, *packets):
    for line in data.split():
        yield Packet(line)
    for packet in packets:
        yield packet


def solve1(data):
    return sum(i + 1 for i, pair in enumerate(get_pairs(data)) if pair[0] <= pair[1])


def solve2(data):
    div1, div2 = Packet("[[2]]"), Packet("[[6]]")
    packets = sorted(get_packets(data, div1, div2))
    return (packets.index(div1) + 1) * (packets.index(div2) + 1)
