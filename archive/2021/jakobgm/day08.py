from pathlib import Path

p = Path("inputs/08.txt")
entries = p.read_text().strip().split("\n")
looking = []
number = 0
for entry in entries:
    patterns, signal = entry.split(" | ")
    patterns = list(map(frozenset, patterns.split()))
    signal = list(map(frozenset, signal.split()))

    seven = next(p for p in patterns if len(p) == 3)
    four = next(p for p in patterns if len(p) == 4)
    one = next(p for p in patterns if len(p) == 2)
    eight = next(p for p in patterns if len(p) == 7)

    top = seven - one
    nine = next(p for p in patterns if len(p) == 6 and len(p - four - top) == 1)

    bottom = nine - four - top
    three = next(p for p in patterns if len(p) == 5 and len(p - seven - bottom) == 1)

    bottom_left = eight - nine
    top_left = eight - three - bottom_left
    middle = four - one - top_left
    zero = eight - middle

    patterns = set(patterns) - set((one, four, seven, eight, nine, three, zero))
    six = next(p for p in patterns if len(p) == 6)

    top_right = eight - six
    five = next(p for p in patterns if len(p) == 5 and top_right.isdisjoint(p))
    two = next(p for p in patterns if p not in (six, five))

    mapping = {
        pattern: str(number)
        for number, pattern
        in enumerate((zero, one, two, three, four, five, six, seven, eight, nine))
    }
    number += int("".join([mapping[s] for s in signal]))

print(number)
