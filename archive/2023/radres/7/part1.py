from helpers import *
with open("input.txt", "r") as f:
    out = f.read()

lines = out.splitlines()

from collections import Counter

def classify_card(card):
    counter = Counter(card)
    most_com = counter.most_common(5)
    if most_com[0][1] == 5:
        return (7, card)
    elif most_com[0][1] == 4:
        return (6, card)
    elif most_com[0][1] == 3 and most_com[1][1] == 2:
        return (5, card)
    elif most_com[0][1] == 3 and most_com[1][1] == 1:
        return (4, card)
    elif most_com[0][1] == 2 and most_com[1][1] == 2:
        return (3, card)
    elif most_com[0][1] == 2 and most_com[1][1] == 1:
        return (2, card)
    elif most_com[0][1] == 1:
        return (1, card)
    assert False, "I was never here"


card_and_bids = []
for i,line in enumerate(lines):
    card, bid = line.split()
    card = card.replace("A", "R")
    card = card.replace("K", "P")
    card = card.replace("Q", "O")
    card = card.replace("J", "N")
    card = card.replace("T", "M")
    card = card.replace("9", "L")
    card = card.replace("8", "K")
    card = card.replace("7", "J")
    card = card.replace("6", "I")
    card = card.replace("5", "H")
    card = card.replace("4", "G")
    card = card.replace("3", "F")
    card = card.replace("2", "E")

    card_and_bids.append((card, int(bid)))


card_and_bids.sort(key=lambda x: classify_card(x[0]))

total = 0
for i, (card, bid) in enumerate(card_and_bids):
    total += bid * (i+1)
print(total)