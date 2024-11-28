from sys import stdin
from collections import defaultdict


def extract_count(rule):
    (count, name) = rule.split(" ", 1)
    count = 0 if count == "no" else int(count)
    return (name.replace("bags", "").replace("bag", "").strip(), count)


lookup = defaultdict(dict) # type: ignore
for line in stdin:
    (color, rules) = line.strip()[:-1].split(" bags contain ")
    contents = dict(extract_count(rule.strip()) for rule in rules.split(", "))
    lookup[color] = contents


def build_graph():
    stack = list(lookup.keys())

    graph = defaultdict(set)  # type: ignore
    visited = set()

    while stack:
        current = stack.pop()
        visited.add(current)

        if current in lookup:
            for child in lookup[current]:
                graph[child].add(current)

                if child not in visited:
                    stack.append(child)

    return graph


def a():
    graph = build_graph()
    stack = ["shiny gold"]

    candidates = set()

    while stack:
        current = stack.pop()
        for candidate in graph[current]:
            candidates.add(candidate)
            stack.append(candidate)

    return len(candidates)


def b():
    def _count(start):
        count = 1
        for (k, v) in lookup[start].items():
            count += v * _count(k)
        return count

    return _count("shiny gold") - 1


print(a())
print(b())
