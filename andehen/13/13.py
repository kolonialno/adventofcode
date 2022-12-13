import functools


with open("input.txt") as f:
    pairs = [[eval(p) for p in p.strip().split("\n")] for p in f.read().split("\n\n")]


def compare(left, right):
    if isinstance(left, int) and isinstance(right, int):
        if left < right:
            return -1
        elif left > right:
            return 1

    if isinstance(left, int) and isinstance(right, list):
        return compare([left], right)

    if isinstance(left, list) and isinstance(right, int):
        return compare(left, [right])

    if isinstance(left, list) and isinstance(right, list):
        left_len, right_len = len(left), len(right)
        for element in range(min(left_len, right_len)):
            result = compare(left[element], right[element])
            if result != 0:
                return result
        if left_len < right_len:
            return -1
        if left_len > right_len:
            return 1

    return 0


in_the_right_order = []
for i, (p1, p2) in enumerate(pairs):
    if compare(p1, p2) < 0:
        in_the_right_order.append(i + 1)

print(sum(in_the_right_order))

all_packets = [p for pair in pairs for p in pair]
all_packets.append([[2]])
all_packets.append([[6]])

sorted_packets = sorted(all_packets, key=functools.cmp_to_key(compare))
print((sorted_packets.index([[2]]) + 1) * (sorted_packets.index([[6]]) + 1))
