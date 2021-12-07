import numpy as np

with open("inputs/07.txt") as f:
    input = [int(i) for i in f.readline().split(",")]


def calculate_fuel(initial_alignment, constant_rate=True):
    min_fuel = float("inf")
    for i in input:
        alignment_goal = np.array([i] * len(initial_alignment))
        diff = abs(initial_alignment - alignment_goal)
        if not constant_rate:
            fuel = sum(diff * (diff + 1) / 2)  # formula: n*(n+1)/2
        else:
            fuel = sum(diff)
        if fuel < min_fuel:
            min_fuel = fuel
    return min_fuel


answer1 = calculate_fuel(input)
answer2 = calculate_fuel(input, constant_rate=False)

print(f"Answer1: {answer1}")  # 348664
print(f"Answer2: {answer2}")  # 100220525.0
