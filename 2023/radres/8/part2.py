from helpers import *
with open("input.txt", "r") as f:
    out = f.read()

# out = """RL

# AAA = (BBB, CCC)
# BBB = (DDD, EEE)
# CCC = (ZZZ, GGG)
# DDD = (DDD, DDD)
# EEE = (EEE, EEE)
# GGG = (GGG, GGG)
# ZZZ = (ZZZ, ZZZ)"""

# out = """LLR

# AAA = (BBB, BBB)
# BBB = (AAA, ZZZ)
# ZZZ = (ZZZ, ZZZ)"""

instruction, rules = out.split("\n\n")

paths = {}
paths_vec = []
print(rules.splitlines())
for rule in rules.splitlines():
    key, value = rule.split("=")
    key = key.strip()
    value = value.split("(")[1].strip(")").split(", ")
    paths[key] = value
    paths_vec.append((key,value))


from itertools import cycle
keys = [key for key in paths.keys() if key.endswith("A")]




stepsvec= []
for key in keys:
    i = 0
    steps = 0
    for c in cycle(instruction):
        if key.endswith("Z"):
            break
        key = paths[key][1] if c == "R" else paths[key][0]

        steps += 1
        i += 1
    stepsvec.append(steps)

import math

def lcm_of_list(numbers):
    lcm = 1
    for number in numbers:
        lcm = math.lcm(lcm, number)
    return lcm

print(lcm_of_list(stepsvec))