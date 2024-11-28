from __future__ import annotations
from pathlib import Path
from typing import Self
import numpy as np
from dataclasses import dataclass

data = Path("day02.txt").read_text().splitlines()

# Part 1

# data = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".split("\n")

# print(data)

line = data[0]


@dataclass
class Game:
    id: int
    red: int = 0
    green: int = 0
    blue: int = 0

    @classmethod
    def from_rounds(cls, rounds: list[Round]) -> Self:
        red = max([round.red for round in rounds])
        green = max([round.green for round in rounds])
        blue = max([round.blue for round in rounds])
        return cls(id=rounds[0].id, red=red, green=green, blue=blue)

    # all that was needed for part 2:
    def power(self):
        return self.red * self.green * self.blue


@dataclass
class Round:
    id: int
    round: int
    red: int = 0
    green: int = 0
    blue: int = 0


bag = Game(0, 12, 13, 14)

games = []
for line in data:
    id_part, cube_part = line.split(":")
    id = int(id_part.split(" ")[1])
    colors: dict[str, int] = {}
    rounds = []
    for round, round_str in enumerate(cube_part.split(";"), 1):
        for cube_str in round_str.split(","):
            number, color = cube_str.strip().split(" ")
            colors[color] = int(number)
        rounds.append(Round(id=id, round=round, **colors))
    games.append(Game.from_rounds(rounds))

ids = []
powers = []
for game in games:
    if (game.red <= bag.red) & (game.green <= bag.green) & (game.blue <= bag.blue):
        ids.append(game.id)
    powers.append(game.power())

print(sum(ids))
print(sum(powers))
