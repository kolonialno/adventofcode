import numpy as np

with open("inputs/07.txt") as f:
    initial_state = np.array([i for i in f.readline().split(",")]).astype(int)


def calculate_fuel(initial_state, constant_rate=True):
    dim1 = initial_state.max()
    dim2 = len(initial_state)
    alignment_goals = np.tile(range(dim1), dim2).reshape(dim2, dim1).T
    diff = abs(initial_state - alignment_goals)
    if not constant_rate:
        fuel = (diff * (diff + 1) / 2).sum(axis=1) # formula: n*(n+1)/2
    else:
        fuel = diff.sum(axis=1)
    return fuel.min()


answer1 = calculate_fuel(initial_state)
answer2 = calculate_fuel(initial_state, constant_rate=False)

print(f"Answer1: {answer1}")  # 348664
print(f"Answer2: {answer2}")  # 100220525.0
