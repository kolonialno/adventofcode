import enum
from pathlib import Path


class Outcome(enum.Enum):
    WIN = 6
    DRAW = 3
    LOSE = 0

    @classmethod
    def from_(cls, string) -> "Outcome":
        return {
            "X": cls.LOSE,
            "Y": cls.DRAW,
            "Z": cls.WIN,
        }[string]


class Move(enum.Enum):
    ROCK = 1
    PAPER = 2
    SCISSOR = 3

    def play(self, other: "Move") -> Outcome:
        if self.wins_against() == other:
            return Outcome.WIN
        elif self is other:
            return Outcome.DRAW
        else:
            return Outcome.LOSE

    def loses_against(self) -> "Move":
        return {
            self.ROCK: self.PAPER,
            self.PAPER: self.SCISSOR,
            self.SCISSOR: self.ROCK,
        }[self]

    def wins_against(self) -> "Move":
        return {
            self.ROCK: self.SCISSOR,
            self.PAPER: self.ROCK,
            self.SCISSOR: self.PAPER,
        }[self]

    @classmethod
    def from_(cls, string) -> "Move":
        return {
            "A": cls.ROCK,
            "B": cls.PAPER,
            "C": cls.SCISSOR,
            "X": cls.ROCK,
            "Y": cls.PAPER,
            "Z": cls.SCISSOR,
        }[string]


problem = Path("input/2.txt").read_text().strip()
task_a = 0
task_b = 0
for round in problem.split("\n"):
    first, second = round.split()
    them, me = Move.from_(first), Move.from_(second)
    task_a += me.value + me.play(them).value

    outcome = Outcome.from_(second)
    if outcome is Outcome.DRAW:
        me = them
    elif outcome is Outcome.WIN:
        me = them.loses_against()
    else:
        me = them.wins_against()
    task_b += me.value + outcome.value

print(f"{task_a = }")
print(f"{task_b = }")
