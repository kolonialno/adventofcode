from collections import defaultdict, deque
from functools import partial
from itertools import islice


def play_memory_game(initial):
    seen = defaultdict(partial(deque, maxlen=2))
    for i, x in enumerate(initial):
        seen[x].append(i + 1)
    turn = len(initial)
    last = initial[-1]
    while True:
        turn += 1
        if len(seen[last]) < 2:
            last = 0
        else:
            last = seen[last][-1] - seen[last][0]

        seen[last].append(turn)
        yield last


def testcase() -> int:
    cases = (
        (
            (0, 3, 6),
            436,
        ),
        (
            (1, 3, 2),
            1,
        ),
        (
            (2, 1, 3),
            10,
        ),
        (
            (1, 2, 3),
            27,
        ),
        (
            (2, 3, 1),
            78,
        ),
        (
            (3, 2, 1),
            438,
        ),
        (
            (3, 1, 2),
            1836,
        ),
    )
    for initial, expected in cases:
        memory_game = play_memory_game(initial)
        result = next(islice(memory_game, 2020 - len(initial) - 1, None))
        print(result, expected)
        assert result == expected


def main() -> int:
    initial = (1, 20, 8, 12, 0, 14)
    memory_game = play_memory_game(initial)
    return next(islice(memory_game, 2020 - len(initial) - 1, None))


def secondary() -> int:
    initial = (1, 20, 8, 12, 0, 14)
    memory_game = play_memory_game(initial)
    return next(islice(memory_game, 30000000 - len(initial) - 1, None))
