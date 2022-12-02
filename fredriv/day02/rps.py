with open("input.txt") as f:
    data = f.read()

def score(outcome: dict[int]) -> int:
    rounds = [line.split(" ") for line in data.split("\n") if line != ""]
    return sum([outcome[opponent][me] for (opponent, me) in rounds])

outcome = {
    'A': {'X': 3 + 1, 'Y': 6 + 2, 'Z': 0 + 3},
    'B': {'X': 0 + 1, 'Y': 3 + 2, 'Z': 6 + 3},
    'C': {'X': 6 + 1, 'Y': 0 + 2, 'Z': 3 + 3},
}

print(f"Part 1: {score(outcome)}")

outcome2 = {
    'A': {'X': 0 + 3, 'Y': 3 + 1, 'Z': 6 + 2},
    'B': {'X': 0 + 1, 'Y': 3 + 2, 'Z': 6 + 3},
    'C': {'X': 0 + 2, 'Y': 3 + 3, 'Z': 6 + 1},
}

print(f"Part 2: {score(outcome2)}")
