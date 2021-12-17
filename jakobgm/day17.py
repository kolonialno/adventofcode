import itertools
from pathlib import Path

import numpy as np


puzzle = (
    int(coordinate)
    for coordinate in
    Path("inputs/17.txt")
    .read_text()
    .strip()
    .replace("target area: x=", "")
    .replace(", y=", "..")
    .split("..")
)
x_min, x_max, y_min, y_max = puzzle
x_target = range(x_min, x_max + 1)
y_target = range(y_min, y_max + 1)


def shoot(x_vel, y_vel):
    x, y = 0, 0
    top = 0
    while x < x_max and y > y_min:
        x += x_vel
        y += y_vel
        x_vel -= np.sign(x_vel)
        y_vel -= 1
        top = max(y, top)
        if x in x_target and y in y_target:
            return top


tops = []
attempts = itertools.product(range(1, x_max + 1), range(y_min, 200))
tops = [shoot(*attempt) for attempt in attempts if shoot(*attempt) is not None]
print(max(tops))
print(len(tops))
