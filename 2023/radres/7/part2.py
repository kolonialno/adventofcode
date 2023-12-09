from helpers import *
with open("input.txt", "r") as f:
    out = f.read()

lines = out.splitlines()

from collections import Counter

def classify_card(card):
    card, card_with_j = card
    counter = Counter(card)
    most_com = counter.most_common(5)
    if most_com[0][1] == 5:
        return (7, card_with_j)
    elif most_com[0][1] == 4:
        return (6, card_with_j)
    elif most_com[0][1] == 3 and most_com[1][1] == 2:
        return (5, card_with_j)
    elif most_com[0][1] == 3 and most_com[1][1] == 1:
        return (4, card_with_j)
    elif most_com[0][1] == 2 and most_com[1][1] == 2:
        return (3, card_with_j)
    elif most_com[0][1] == 2 and most_com[1][1] == 1:
        return (2, card_with_j)
    elif most_com[0][1] == 1:
        return (1, card_with_j)
    assert False, "I was never here"

def get_J_replacement(card):
    j_count = card.count("D")
    card = list(filter(lambda x: x != "D", card))
    if j_count == 0:
        return "-"
    if j_count == 5:
        return "R"

    mcs = Counter(card).most_common(5)
    mcs.sort(key = lambda x : (x[1], x[0]), reverse=True)
    return mcs[0][0]


def card_replace(card):
    card = card.replace("J", "D")
    card = card.replace("A", "R")
    card = card.replace("K", "P")
    card = card.replace("Q", "O")
    card = card.replace("T", "M")
    card = card.replace("9", "L")
    card = card.replace("8", "K")
    card = card.replace("7", "J")
    card = card.replace("6", "I")
    card = card.replace("5", "H")
    card = card.replace("4", "G")
    card = card.replace("3", "F")
    card = card.replace("2", "E")
    return card

card_and_bids = []
for i,line in enumerate(lines):
    card, bid = line.split()
    assert(len(card) == 5)

    card = card_replace(card)
    card_with_j = card
    j_replacement = get_J_replacement(card)
    card = card.replace("D", j_replacement)

    card_and_bids.append(((card, card_with_j), int(bid)))



card_and_bids.sort(key=lambda x: classify_card(x[0]))

for card, bid in card_and_bids:
    card = card[0]

total = 0
for i, (card, bid) in enumerate(card_and_bids):
    total += bid * (i+1)

print(total)


