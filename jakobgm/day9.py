from pathlib import Path

import numpy as np

instructions = Path("./input/9.txt").read_text().strip().splitlines()


def tail_positions(num_knots: int) -> int:
    knots = [np.array([0, 0]) for _ in range(num_knots)]
    tail_positions = {(0, 0)}
    for instruction in instructions:
        letter, steps = instruction.split()
        direction = {
            "L": [-1, 0],
            "R": [1, 0],
            "U": [0, 1],
            "D": [0, -1],
        }[letter]
        for _ in range(int(steps)):
            knots[0] += direction
            for head, tail in zip(knots[0:], knots[1:]):
                if all(-1 <= h - t <= 1 for h, t in zip(head, tail)):
                    continue
                tail += (head - tail).clip(-1, 1)
            tail_positions.add(tuple(knots[-1]))
    return len(tail_positions)


print(tail_positions(2))
print(tail_positions(10))
