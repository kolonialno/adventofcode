from collections import defaultdict
from functools import cmp_to_key

# Read input
rules, updates = open(0).read().strip().split("\n\n")

# Part 1
constraints = defaultdict(list)
for rule in rules.split("\n"):
    a, b = rule.split("|")
    constraints[a].append(b)

total = 0
unordered = []
for update in updates.split("\n"):
    buffer = []
    for page in (pages := update.split(",")):
        if any(successor in buffer for successor in constraints[page]):
            unordered.append(pages)
            break
        buffer.append(page)
    else:
        total += int(pages[len(pages) // 2])

print("Part 1:", total)

# Part 2
total = 0
for pages in unordered:
    pages.sort(key=cmp_to_key(lambda a, b: -1 if b in constraints[a] else 1))
    total += int(pages[len(pages) // 2])

print("Part 2:", total)
