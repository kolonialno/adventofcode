from sys import stdin
from collections import Counter, defaultdict, deque
from functools import cache

lines = [int(line.strip()) for line in stdin]

joltages = [0] + sorted(lines)
joltages.append(joltages[-1] + 3)

def a():
    diff_counter = Counter()

    for (a, b) in zip(joltages, joltages[1:]):
        diff_counter[b - a] += 1

    return diff_counter[1] * diff_counter[3]

def build_graph():
    graph = defaultdict(set)

    for i in range(len(joltages)):
        current = joltages[i]
        for j in range(1, min(4, len(joltages) - i)):
            neighbor = joltages[i + j]
            if neighbor - current < 4:
                graph[current].add(neighbor)
        
    return graph

# Works on example input
# Won't finish anytime soon on real input 
def b_insane():
    graph = build_graph()
    queue = deque([0])

    goal = joltages[-1]

    count = 0

    while queue:
        current = queue.popleft()

        if current == goal:
            print(current)
            count += 1

        for neighbor in graph[current]:
            queue.append(neighbor)
        
    
    return count


# Attempt 2: Really fast on all inputs
def b():
    graph = build_graph()

    inverted = defaultdict(set)
    for k, v in graph.items():
        for x in v:
            inverted[x].add(k)


    @cache
    def count_paths_to(k):
        if k == 0:
            return 1
        return sum(count_paths_to(x) for x in inverted[k])
    
    return count_paths_to(joltages[-1])

print(a())
print(b())
