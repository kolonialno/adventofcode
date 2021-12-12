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
unfinished_paths = [["start", neighbour] for neighbour in graph.neighbors("start")]

while unfinished_paths:
    path = unfinished_paths.pop()
    for neighbour in graph.neighbors(path[-1]):
        if neighbour == "start":
            continue
        elif neighbour == "end":
            finished.append(path.copy() + [neighbour])
        elif neighbour.upper() == str(neighbour):
            unfinished_paths.append(path.copy() + [neighbour])
        elif neighbour not in path:
            unfinished_paths.append(path.copy() + [neighbour])

print(len(finished))
