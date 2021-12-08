from sys import stdin


def parse(line):
    signals, outputs = line.split(" | ")
    return (
        [frozenset(s) for s in signals.split()],
        [frozenset(o) for o in outputs.split()],
    )


entries = [parse(line.strip()) for line in stdin]

#   0:      1:      2:      3:      4:
#  aaaa    ....    aaaa    aaaa    ....
# b    c  .    c  .    c  .    c  b    c
# b    c  .    c  .    c  .    c  b    c
#  ....    ....    dddd    dddd    dddd
# e    f  .    f  e    .  .    f  .    f
# e    f  .    f  e    .  .    f  .    f
#  gggg    ....    gggg    gggg    ....

#   5:      6:      7:      8:      9:
#  aaaa    aaaa    aaaa    aaaa    aaaa
# b    .  b    .  .    c  b    c  b    c
# b    .  b    .  .    c  b    c  b    c
#  dddd    dddd    ....    dddd    dddd
# .    f  e    f  .    f  e    f  .    f
# .    f  e    f  .    f  e    f  .    f
#  gggg    gggg    ....    gggg    gggg

SEGMENTS = {
    0: 6,
    1: 2,
    2: 5,
    3: 5,
    4: 4,
    5: 5,
    6: 6,
    7: 3,
    8: 7,
    9: 6,
}


def a():
    total = 0
    for _, outputs in entries:
        for output in outputs:
            for n in (1, 4, 7, 8):
                if len(output) == SEGMENTS[n]:
                    total += 1
                    break

    return total


def b():
    total = 0

    aaaa = lambda x: x[7] - x[1]
    gggg = lambda x: x[9] - x[4] - aaaa(x)
    ee = lambda x: x[8] - x[9]
    bb = lambda x: x[8] - x[3] - ee(x)
    dddd = lambda x: x[4] - x[1] - bb(x)
    cc = lambda x: x[8] - x[6]

    for signals, outputs in entries:

        def consume(pred_or_constant):
            value = (
                next(s for s in signals if pred_or_constant(s))
                if callable(pred_or_constant)
                else pred_or_constant
            )
            signals.remove(value)
            return value

        m = {}
        m[1] = consume(lambda s: len(s) == SEGMENTS[1])
        m[4] = consume(lambda s: len(s) == SEGMENTS[4])
        m[7] = consume(lambda s: len(s) == SEGMENTS[7])
        m[8] = consume(lambda s: len(s) == SEGMENTS[8])
        m[9] = consume(lambda s: len(s) == SEGMENTS[9] and len(s - m[4] - aaaa(m)) == 1)
        m[3] = consume(lambda s: len(s) == SEGMENTS[3] and len(s - m[7] - gggg(m)) == 1)
        m[0] = consume(m[8] - dddd(m))
        m[6] = consume(lambda s: len(s) == SEGMENTS[6])
        m[2] = consume(lambda s: len(s) == SEGMENTS[2] and (cc(m) & s))
        m[5] = consume(lambda s: len(s) == SEGMENTS[5])

        lookup = {signal: str(number) for number, signal in m.items()}
        total += int("".join([lookup[output] for output in outputs]))

    return total


print(a())
print(b())
