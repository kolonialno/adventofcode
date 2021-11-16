from sys import stdin

lines = [line.strip() for line in stdin]


def compute_id(line):
    def _scan(letters, size):
        start, end = 0, size

        for letter in letters:
            if letter == "F" or letter == "L":
                end = (start + end) // 2
            elif letter == "B" or letter == "R":
                start = (start + end) // 2 + 1

        return max(start, end)

    row = _scan(line[0:7], 127)
    column = _scan(line[7:10], 7)

    return row * 8 + column


ids = [compute_id(line.strip()) for line in lines]


def a():
    return max(ids)


def b():
    sorted_ids = sorted(ids)

    for i in range(1, len(sorted_ids)):
        if sorted_ids[i - 1] != sorted_ids[i] - 1:
            return sorted_ids[i] - 1

    return -1


assert compute_id("FBFBBFFRLR") == 357
assert compute_id("BFFFBBFRRR") == 567
assert compute_id("FFFBBBFRRR") == 119
assert compute_id("BBFFBBFRLL") == 820

print(a())
print(b())
