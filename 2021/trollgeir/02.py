import pandas as pd

data = pd.read_csv("inputs/02.txt", header=None, names=["action", "magnitude"], sep=" ")

forward_actions = data["action"] == "forward"
up_actions = data["action"] == "up"

# Negate magnitude of all 'up' actions
data.loc[up_actions, "magnitude"] *= -1

# Puzzle 1
forward_sum = data.loc[forward_actions, "magnitude"].sum()
horizontal_sum = data.loc[~forward_actions, "magnitude"].sum()

answer1 = forward_sum * horizontal_sum

# Puzzle 2

# Set horizontal state
data["horizontal_position"] = data.loc[forward_actions, "magnitude"].cumsum()

# Set aim state
data["aim"] = data.loc[~forward_actions, "magnitude"].cumsum()
data["aim"] = data["aim"].fillna(method="ffill").fillna(0)

# Set depth state
data["depth"] = (
    data[forward_actions]["aim"] * data[forward_actions]["magnitude"]
).cumsum()

# Get the last states
last_states = data.apply(lambda x: x[x.notnull()].values[-1])

answer2 = last_states["horizontal_position"] * last_states["depth"]

print(f"Answer1: {answer1}")  # 1762050
print(f"Answer2: {answer2}")  # 1855892637.0
