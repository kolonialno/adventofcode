import string
import networkx as nx

with open("input.txt") as f:
    grid = [[c for c in r] for r in f.read().strip().split("\n")]

G = nx.DiGraph()
nr, nc = len(grid), len(grid[0])


def get_height(c):
    if c == "S":
        c = "a"
    if c == "E":
        c = "z"
    return string.ascii_letters.index(c)


def maybe_add_edge(h, i, j, di, dj):
    global grid, G
    dh = get_height(grid[i + di][j + dj])
    if dh <= (h + 1):
        G.add_edge((i, j), (i + di, j + dj))


start = None
end = None
a_positions = []

for i, row in enumerate(grid):
    for j, col in enumerate(row):
        G.add_node((i,j))
        c = grid[i][j]
        if c == "S":
            start = (i, j)
        if c == "E":
            end = (i, j)
        h = get_height(c)

        if h == 0:
            a_positions.append((i, j))

        # Up
        if i > 0:
            maybe_add_edge(h, i, j, -1, 0)
        # Down
        if i < nr - 1:
            maybe_add_edge(h, i, j, 1, 0)
        # Left
        if j > 0:
            maybe_add_edge(h, i, j, 0, -1)
        # Right
        if j < nc - 1:
            maybe_add_edge(h, i, j, 0, 1)

print(nx.shortest_path_length(G, source=start, target=end))

shortest = 99999
for i, j in a_positions:
    try:
        candidate = nx.shortest_path_length(G, source=(i, j), target=end)
    except nx.NetworkXException:
        candidate = 99999
    if candidate < shortest:
        shortest = candidate

print(shortest)
