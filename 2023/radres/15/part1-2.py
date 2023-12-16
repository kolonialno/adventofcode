from helpers import *
import os
import time
with open("input.txt", "r") as f:
    out = f.read()

out = out.replace("\n", "")
# out = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"


def hash(text):
    current_value = 0
    for c in text:
        current_value += ord(c)
        current_value *= 17
        current_value %= 256
    return current_value

result = 0
for word in out.split(","):
    result += hash(word)
print("part1: ", result)

boxes = [[] for _ in range(256)]
focals = {}
for word in out.split(","):
    if "-" in word:
        key = word.split("-")[0]
        ind = hash(key)
        if key in boxes[ind]:
            boxes[ind].remove(key)
        if key in focals:
            focals.pop(key)
    elif "=" in word:
        key, focal = word.split("=")
        ind = hash(key)
        if not key in boxes[ind]:
            boxes[ind].append(key)
        focals[key] = int(focal)
    else:
        assert False, "nope"

powers = 0
for key,value in focals.items():
    ind = hash(key)
    power = (boxes[ind].index(key)+1)*value*(ind+1)
    powers += power
print("part2: ", powers)