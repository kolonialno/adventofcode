S = {"A": 1, "B": 2, "C": 3, "X": 1, "Y": 2, "Z": 3}


def score_p1(p1, p2):

    d = S[p2] - S[p1]
    s = S[p2]

    if d == 0: return 3 + s # draw
    if d in [1, -2]: return 6 + s # win
    return s # loose


def score_p2(p1, o):
    if o == "X": return S[p1] + 2 - 3 * (1 % S[p1]) # loose
    if o == "Y": return 3 + S[p1] # draw
    return 6 + (S[p1] % 3 + 1) # win


with open("input.txt") as f:
    games = [g.split(" ") for g in f.read().split("\n")][:-1]

print(sum([score_p1(p1, p2) for p1, p2 in games]))
print(sum([score_p2(p1, o) for p1, o in games]))
