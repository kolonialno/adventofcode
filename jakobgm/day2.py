from pathlib import Path

problem = Path("input/2.txt").read_text().strip()

MAP = {
    "A": "rock",
    "B": "paper",
    "C": "scissor",
    "X": "rock",
    "Y": "paper",
    "Z": "scissor",
}

POINTS = {
    "rock": 1,
    "paper": 2,
    "scissor": 3,
    "lose": 0,
    "draw": 3,
    "win": 6,
}


def resolve(them, me):
    if them == me:
        return 3 + POINTS[me]
    elif (
        (them == "scissor" and me == "rock")
        or (them == "paper" and me == "scissor")
        or (them == "rock" and me == "paper")
    ):
        return 6 + POINTS[me]
    else:
        return POINTS[me]


points = 0
for round in problem.split("\n"):
    them, me = round.split()
    result = resolve(MAP[them], MAP[me])
    points += result

print(points)


MAP = {
    "A": "rock",
    "B": "paper",
    "C": "scissor",
    "X": "lose",
    "Y": "draw",
    "Z": "win",
}

LOSE = {
    "rock": "scissor",
    "paper": "rock",
    "scissor": "paper",
}
WIN = {
    "rock": "paper",
    "paper": "scissor",
    "scissor": "rock",
}


def elaborate(them, outcome):
    if outcome == "draw":
        return POINTS[them] + POINTS[outcome]
    elif outcome == "win":
        return POINTS[WIN[them]] + POINTS[outcome]
    elif outcome == "lose":
        return POINTS[LOSE[them]] + POINTS[outcome]
    else:
        raise


points = 0
for round in problem.split("\n"):
    them, me = round.split()
    result = elaborate(MAP[them], MAP[me])
    points += result

print(points)
