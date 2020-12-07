import re
from collections import deque

OUTER_RE = re.compile("(\w+ \w+) bags? contain (.*).")
INNTER_RE = re.compile("(\d+) (\w+ \w+) bags?")
rules = [(OUTER_RE.match(rule).group(1), OUTER_RE.match(rule).group(2)) for rule in open("rules.txt").read().strip().split("\n")]
rules = dict(
    (outer, [(int(match.group(1)), match.group(2)) for match in re.finditer(INNTER_RE, inner)]) for outer, inner in rules
)

bags = {}
for outer, inner in rules.items():
    queue = deque(inner)
    bags[outer] = []
    while queue:
        count, item = queue.popleft()
        bags[outer].append((count, item))
        queue.extend(
            (count * inner_count, inner_item) for inner_count, inner_item in rules[item]
        )

# Part 1
print(sum(any(item == 'shiny gold' for _, item in items) for items in bags.values()))

# Part 2
print(sum(c for c, _ in bags["shiny gold"]))
