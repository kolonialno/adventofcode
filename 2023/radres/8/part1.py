from helpers import *
with open("input.txt", "r") as f:
    out = f.read()

instruction, rules = out.split("\n\n")

paths = {}
paths_vec = []
for rule in rules.splitlines():
    key, value = rule.split("=")
    key = key.strip()
    value = value.split("(")[1].strip(")").split(", ")
    paths[key] = value
    paths_vec.append((key,value))


key = "AAA"

steps = 0
from itertools import cycle
for c in cycle(instruction):
    if key == "ZZZ":
        break

    assert c == "R" or c == "L"
    key = paths[key][1] if c == "R" else paths[key][0]
    

    steps+=1
print(steps)




