from pathlib import Path
from enum import Enum, auto

class AbstractHand:
    hand_value = 0
    def __or__(self, other):
        "Overload | operator to return score of `handA | handB`."
        if self > other:
            points =  6
        elif self == other:
            points =  3
        else:
            points =  0
        return self.hand_value + points

class Rock(AbstractHand):
    hand_value = 1
    def __eq__(self, other):
        return isinstance(other, Rock)
    def __gt__(self, other):
        return isinstance(other, Scissors)
    def __ls__(self, other):
        return isinstance(other, Paper)

class Paper(AbstractHand):
    hand_value = 2
    def __eq__(self, other):
        return isinstance(other, Paper)
    def __gt__(self, other):
        return isinstance(other, Rock)
    def __ls__(self, other):
        return isinstance(other, Scissors)

class Scissors(AbstractHand):
    hand_value = 3
    def __eq__(self, other):
        return isinstance(other, Scissors)
    def __gt__(self, other):
        return isinstance(other, Paper)
    def __ls__(self, other):
        return isinstance(other, Rock)


r, p, s = Rock(), Paper(), Scissors()

map_first_symbol_to_hand = {
    "A":r,
    "B":p,
    "C":s,
}

map_second_symbol_to_hand_part1 = {
    "X":r,
    "Y":p,
    "Z":s,
}

def map_second_symbol_to_hand_part2(their_hand, our_symbol):
    hands = [r, p, s]
    for hand in hands:
        if our_symbol == "X":
            if hand < their_hand:
                break
        elif our_symbol == "Y":
            if hand == their_hand:
                break
        else:
            if hand > their_hand:
                break
    return hand

data = """A Y
B X
C Z
"""

data = Path("day02/input").read_text()

def score1(data):
    total_score = 0
    for them, _space, us in data.splitlines():
        their_hand = map_first_symbol_to_hand[them]
        our_hand = map_second_symbol_to_hand_part1[us]
        # now fight!
        total_score += our_hand | their_hand
    return total_score

def score2(data):
    total_score = 0
    for them, _space, us in data.splitlines():
        their_hand = map_first_symbol_to_hand[them]
        our_hand = map_second_symbol_to_hand_part2(their_hand, us)
        # now fight!
        total_score += our_hand | their_hand
    return total_score

print(score1(data))
print(score2(data))