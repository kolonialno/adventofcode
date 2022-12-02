with open("input.txt") as f:
    games = [g.split(" ") for g in f.read().split("\n")][:-1]


M = {
    "A": "R",
    "B": "P",
    "C": "S",
    "X": "R",
    "Y": "P",
    "Z": "S",
}

S = {
    "R": 1,
    "P": 2,
    "S": 3
}

def score(P1, P2):
    # Draw
    if P1 == P2:
        return 3 + S[P2]

    if P1 == "R":
        # Loss
        if P2 != "P":
            return S[P2]
        # win
        return 6 + S[P2]

    if P1 == "P":
        # Loss
        if P2 != "S":
            return S[P2]
        # win
        return 6 + S[P2]

    if P1 == "S":
        # Loss
        if P2 != "R":
            return S[P2]
        # win
        return 6 + S[P2]


def score_p1(p1, p2):
    P1 = M[p1]
    P2 = M[p2]

    return score(P1, P2)


# X loose
# Y Draw
# Z win

A = {
        "R": {"X": "S", "Y": "R", "Z": "P"},
        "P": {"X": "R", "Y": "P", "Z": "S"},
        "S": {"X": "P", "Y": "S", "Z": "R"},
}


def score_p2(p1, p2):
    P1 = M[p1]
    P2 = A[P1][p2]
    return score(P1, P2)

scores_p1 = [score_p1(p1, p2) for p1, p2 in games]
print(sum(scores_p1))

scores_p2 = [score_p2(p1, p2) for p1, p2 in games]
print(sum(scores_p2))
