import re
from itertools import product

lines = open("init.txt").read().strip().split("\n")

mask = lines[0].split(" = ")[1]
mem = {}
for line in lines[1:]:
    if line.startswith("mask"):
        mask = line.split(" = ")[1]
        continue

    m = re.match("mem\[(\d+)\] = (\d+)", line)
    address, value = list("{0:b}".format(int(m.group(1))).zfill(36)), int(m.group(2))

    # Part 1
    # for idx, v in enumerate(mask):
    #   if v == "X":
    #       continue
    #   value[idx] = v
    # mem[address] = int("".join(value), 2)

    # Part 2
    for combo in list(product([0, 1], repeat=sum(1 for v in mask if v == "X"))):
        c = 0
        for idx, v in enumerate(mask):
            if v == "X":
                address[idx] = str(combo[c])
                c += 1
            elif v == "1":
                address[idx] = v

        mem[int("".join(address), 2)] = value

print(sum(v for v in mem.values()))
