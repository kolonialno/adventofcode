import re
from collections import defaultdict
from functools import reduce

groups = open("tickets.txt").read().strip().split("\n\n")

rules = {}
for line in groups[0].split("\n"):
    k, r1, r2 = re.match("(.+)\: (\d+-\d+) or (\d+-\d+)", line).groups()
    rules[k] = [
        (int(r1.split("-")[0]), int(r1.split("-")[1])),
        (int(r2.split("-")[0]), int(r2.split("-")[1])),
    ]

your_ticket = [int(v) for v in groups[1].split("\n")[1].split(",")]
nearby_tickets = [[int(v) for v in g.split(",")] for g in groups[2].split("\n")[1:]]

# Part 1
# nearby_ticket_fields = [v for g in nearby_tickets for v in g]
# print(sum(t for t in nearby_ticket_fields if not any(any(rmin <= t <= rmax for rmin, rmax in ranges) for ranges in rules.values())))

# Part 2
valid_tickets = [t for t in nearby_tickets if all(any(any(rmin <= tf <= rmax for rmin, rmax in ranges) for ranges in rules.values()) for tf in t)]

# Find all valid positions a certain "rule" can be
vrules = defaultdict(set)
for idx in range(len(valid_tickets[0])):
    vals = [t[idx] for t in valid_tickets]
    for rule, ranges in rules.items():
        if all(any(rmin <= v <= rmax for rmin, rmax in ranges) for v in vals):
            vrules[rule].add(idx)

# Find the actual position
krules = {}
while vrules.items():
    ck, cv = [(k, list(v)[0]) for k, v in vrules.items() if len(v) == 1][0]
    krules[ck] = cv
    for k in vrules.keys():
        vrules[k].discard(cv)
        vrules = {k: v for k, v in vrules.items() if v}

print(reduce(lambda x, y: x * y, (your_ticket[v] for k, v in krules.items() if k.startswith("departure"))))
