from __future__ import annotations

from enum import StrEnum
from pathlib import Path


class Hand(StrEnum):
    ROCK = "rock"
    PAPER = "paper"
    SCISSORS = "scissors"

    @classmethod
    def from_abc(cls, value: str) -> Hand:
        return {
            "A": Hand.ROCK,
            "B": Hand.PAPER,
            "C": Hand.SCISSORS,
        }[value]

    @classmethod
    def from_xyz_hand(cls, value: str) -> Hand:
        return {
            "X": Hand.ROCK,
            "Y": Hand.PAPER,
            "Z": Hand.SCISSORS,
        }[value]

    @classmethod
    def from_xyz_result(cls, value: str, opponent: Hand) -> Hand:
        match value:
            case "X":  # Loose
                return opponent.defeats
            case "Y":  # Draw
                return opponent
            case "Z":  # Win
                return opponent.looses_to
        assert False

    def __gt__(self, other) -> bool:
        assert isinstance(other, Hand)
        return other == self.defeats

    @property
    def defeats(self) -> Hand:
        return {
            Hand.ROCK: Hand.SCISSORS,
            Hand.PAPER: Hand.ROCK,
            Hand.SCISSORS: Hand.PAPER,
        }[self]

    @property
    def looses_to(self) -> Hand:
        return {
            Hand.ROCK: Hand.PAPER,
            Hand.PAPER: Hand.SCISSORS,
            Hand.SCISSORS: Hand.ROCK,
        }[self]

    @property
    def shape_score(self) -> int:
        return {
            Hand.ROCK: 1,
            Hand.PAPER: 2,
            Hand.SCISSORS: 3,
        }[self]


def solve_a(data: str) -> int:
    points = 0

    for line in data.splitlines():
        abc, xyz = line.split()
        opponent = Hand.from_abc(abc)
        you = Hand.from_xyz_hand(xyz)

        if you == opponent:
            points += 3
        elif you > opponent:
            points += 6

        points += you.shape_score

    return points


def solve_b(data: str) -> int:
    points = 0

    for line in data.splitlines():
        abc, xyz = line.split()
        opponent = Hand.from_abc(abc)
        you = Hand.from_xyz_result(xyz, opponent)

        if you == opponent:
            points += 3
        elif you > opponent:
            points += 6

        points += you.shape_score

    return points


def test() -> None:
    data = Path("test02.txt").read_text()
    assert solve_a(data) == 15
    assert solve_b(data) == 12


if __name__ == "__main__":
    data = Path("input02.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
