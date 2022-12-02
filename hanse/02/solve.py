from sys import stdin

lines = [line.strip().split() for line in stdin]

rules = {
    "rock": ("scissor", 1),
    "paper": ("rock", 2),
    "scissor": ("paper", 3),
}

encodings = {
    "A": "rock",
    "B": "paper",
    "C": "scissor",
    "X": "rock",
    "Y": "paper",
    "Z": "scissor",
}


def a():
    scores = []

    for line in lines:
        a, b = line
        them = encodings[a]
        me = encodings[b]

        if them == me:
            scores.append(rules[me][1] + 3)
        elif rules[me][0] == them:
            scores.append(rules[me][1] + 6)
        elif rules[them][0] == me:
            scores.append(rules[me][1] + 0)

    return sum(scores)


def b():
    scores = []
    for line in lines:
        a, b = line
        them = encodings[a]

        if b == "Y":  # draw
            me = them
            scores.append(rules[them][1] + 3)
        elif b == "Z":  # win
            me = next(x for x, y in rules.items() if y[0] == them)
            scores.append(rules[me][1] + 6)
        elif b == "X":  # lose
            me = rules[them][0]
            scores.append(rules[me][1] + 0)

    return sum(scores)


print(a())
print(b())
