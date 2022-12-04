def gamescore(game: str):
    i1, i2 = game.split(' ')
    return combinations[i1][i2] + input_score[i2]

with open("input.txt", "r") as f:
    data = f.read().strip()

# case 1
combinations = {'A': {'X': 3, 'Y': 6, 'Z': 0},
    'B': {'X': 0, 'Y': 3, 'Z': 6},
    'C': {'X': 6, 'Y': 0, 'Z': 3}}
input_score = {'X': 1, 'Y': 2, 'Z': 3}

print(sum(list(map(gamescore, data.split("\n")))))

# case 2
combinations = {'A': {'X': 3, 'Y': 1, 'Z': 2},
    'B': {'X': 1, 'Y': 2, 'Z': 3},
    'C': {'X': 2, 'Y': 3, 'Z': 1}}
input_score = {'X': 0, 'Y': 3, 'Z': 6}
print(sum(list(map(gamescore, data.split("\n")))))
