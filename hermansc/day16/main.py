import re
import math
from collections import defaultdict

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
print(sum(t for t in [v for g in nearby_tickets for v in g] if not any(any(rmin <= t <= rmax for rmin, rmax in ranges) for ranges in rules.values())))

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
for k, v in sorted(vrules.items(), key=lambda x: len(x[1])):
    krules[k] = list(v - set(krules.values()))[0]

print(math.prod(your_ticket[v] for k, v in krules.items() if k.startswith("departure")))
