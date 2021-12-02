import pandas as pd

data = pd.read_csv("inputs/01.txt", header=None)[0]

# Puzzle 1
answer1 = (data.shift(-1) > data).sum()

# Puzzle 2
rolled = data.rolling(window=3).sum().dropna()
answer2 = (rolled.shift(-1) > rolled).sum()

print(f"Answer1: {answer1}")
print(f"Answer2: {answer2}")
