from pathlib import Path
from typing import Counter


PROBLEM = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
""".strip()
PROBLEM = Path("input/7.txt").read_text().strip()

ORDERING = "AKQJT98765432"
STRENGTH = {card: strength for strength, card in enumerate(reversed(ORDERING))}


class Hand:
    def __init__(self, line: str) -> None:
        self.hand, bid = line.split()
        self.bid = int(bid)
        self.strengths = tuple(STRENGTH[card] for card in self.hand)

        count = Counter(self.hand)
        match list(sorted(count.values(), reverse=True)):
            case [5]:  # Full house
                self.kind = 6
            case [4, 1]:  # Four of a kind
                self.kind = 5
            case [3, 2]:  # Full house
                self.kind = 4
            case [3, 1, 1]:  # Three of a kind
                self.kind = 3
            case [2, 2, 1]:  # Two pairs
                self.kind = 2
            case [2, 1, 1, 1]:  # One pair
                self.kind = 1
            case [1, 1, 1, 1, 1]:  # High card
                self.kind = 0
            case _:
                raise RuntimeError()
        self.sort_key = (self.kind,) + self.strengths

    def __repr__(self) -> str:
        return f"{self.hand}"


hands = []
for line in PROBLEM.splitlines():
    hands.append(Hand(line=line))

hands.sort(key=lambda hand: hand.sort_key)
winnings = 0
for rank, hand in enumerate(hands):
    print(rank + 1, hand.bid, hand)
    winnings += (rank + 1) * hand.bid

print(winnings)


# --- Part two ---
NEW_ORDERING = "AKQT98765432J"
NEW_STRENGTH = {card: strength for strength, card in enumerate(reversed(NEW_ORDERING))}


class NewHand:
    def __init__(self, line: str) -> None:
        self.hand, bid = line.split()
        self.bid = int(bid)
        self.strengths = tuple(NEW_STRENGTH[card] for card in self.hand)

        count = Counter(self.hand.replace("J", ""))
        match list(sorted(count.values(), reverse=True)):
            case [] | [_]:  # Full house
                self.kind = 6
            case [_, 1]:  # Four of a kind
                self.kind = 5
            case [_, 2]:  # Full house
                self.kind = 4
            case [_, 1, 1]:  # Three of a kind
                self.kind = 3
            case [_, _, 1]:  # Two pairs
                self.kind = 2
            case [_, 1, 1, 1]:  # One pair
                self.kind = 1
            case [1, 1, 1, 1, 1]:  # High card
                self.kind = 0
            case _:
                raise RuntimeError()
        self.sort_key = (self.kind,) + self.strengths


new_hands = []
for line in PROBLEM.splitlines():
    new_hands.append(NewHand(line=line))

new_hands.sort(key=lambda hand: hand.sort_key)
new_winnings = 0
for rank, new_hand in enumerate(new_hands):
    new_winnings += (rank + 1) * new_hand.bid

print(new_winnings)
