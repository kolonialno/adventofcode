ROCK = "X"
PAPER = "Y"
SCISSORS = "Z"

SCORE_LOSS = 0
SCORE_DRAW = 3
SCORE_WIN = 6

WIN_GOAL = "Z"
DRAW_GOAL = "Y"
LOSS_GOAL = "X"

move_scores = {
    ROCK: 1,
    PAPER: 2,
    SCISSORS: 3
}

translate_moves = {
    "A": ROCK,
    "B": PAPER,
    "C": SCISSORS,
}

def get_round_score(opponent_move:str , player_move:str):
    if opponent_move == player_move:
        return SCORE_DRAW
    elif opponent_move == ROCK:
        if player_move == PAPER:
            return SCORE_WIN
        return SCORE_LOSS
    elif opponent_move == PAPER:
        if player_move == SCISSORS:
            return SCORE_WIN
        return SCORE_LOSS
    elif opponent_move == SCISSORS:
        if player_move == ROCK:
            return SCORE_WIN
        return SCORE_LOSS
    else:
        raise Exception(f"get_rount_score: should not happen {opponent_move} - {player_move}")


with open("input.txt") as f:
    data = f.read().split('\n')

rounds = [l.split(' ') for l in data if l != ""]

score_acc_p1 = 0
for round in rounds:
    opponent_move = translate_moves[round[0]]
    player_move = round[1]
    score_acc_p1 = score_acc_p1 + get_round_score(opponent_move, player_move) + move_scores[player_move]

print(f"Part 1: {score_acc_p1}")

def get_needed_move(opponent_move, goal):
    if goal == DRAW_GOAL:
        return opponent_move
    elif goal == WIN_GOAL:
        if opponent_move == PAPER:
            return SCISSORS
        elif opponent_move == ROCK:
            return PAPER
        elif opponent_move == SCISSORS:
            return ROCK
    elif goal == LOSS_GOAL:
        if opponent_move == PAPER:
            return ROCK
        elif opponent_move == ROCK:
            return SCISSORS
        elif opponent_move == SCISSORS:
            return PAPER
    raise Exception(f"get_needed_move: should not happen {opponent_move} - {goal}")

score_acc_p2 = 0
for round in rounds:
    opponent_move = translate_moves[round[0]]
    goal = round[1]
    player_move = get_needed_move(opponent_move, goal)
    score_acc_p2 = score_acc_p2 + get_round_score(opponent_move, player_move) + move_scores[player_move]

print(f"Part 2: {score_acc_p2}")

