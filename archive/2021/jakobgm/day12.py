from pathlib import Path

from networkx import Graph

edges = (
    edge.split("-")
    for edge
    in Path("inputs/12.txt").read_text().strip().split("\n")
)
graph = Graph()
graph.add_edges_from(edges)

finished = []
unfinished_paths = [(False, ["start", neighbour]) for neighbour in graph.neighbors("start")]

task_b = True
while unfinished_paths:
    has_double_visit, path = unfinished_paths.pop()
    for neighbour in graph.neighbors(path[-1]):
        if neighbour == "start":
            continue
        elif neighbour == "end":
            finished.append(path.copy() + [neighbour])
        elif neighbour.upper() == str(neighbour):
            unfinished_paths.append((has_double_visit, path.copy() + [neighbour]))
        elif neighbour not in path:
            unfinished_paths.append((has_double_visit, path.copy() + [neighbour]))
        elif not has_double_visit and task_b:
            unfinished_paths.append((True, path.copy() + [neighbour]))

print(len(finished))
