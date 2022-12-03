
_score = {
    "A X":1+3, 
    "B X":1+0,
    "C X":1+6,

    "A Y":2+6,
    "B Y":2+3,
    "C Y":2+0,

    "A Z":3+0,
    "B Z":3+6,
    "C Z":3+3,
}

score = {
    "A X":3+0, 
    "B X":1+0,
    "C X":2+0,

    "A Y":1+3,
    "B Y":2+3,
    "C Y":3+3,

    "A Z":2+6,
    "B Z":3+6,
    "C Z":1+6,
}

with open("input.txt") as file:
    print(sum(score[line.rstrip()] for line in file.readlines()))
