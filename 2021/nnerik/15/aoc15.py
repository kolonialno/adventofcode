import networkx as nx

with open("input.txt") as f:
    input = [[int(risk) for risk in line.strip()] for line in f]
    m, n = len(input[0]), len(input)


def risk(x, y):
    return (input[y % n][x % m] + x // m + y // n - 1) % 9 + 1


def solve(size=1):
    cave = nx.DiGraph()
    for y in range(n * size):
        for x in range(m * size):
            if x > 0:
                cave.add_edge((x - 1, y), (x, y), weight=risk(x, y))
                cave.add_edge((x, y), (x - 1, y), weight=risk(x - 1, y))
            if y > 0:
                cave.add_edge((x, y - 1), (x, y), weight=risk(x, y))
                cave.add_edge((x, y), (x, y - 1), weight=risk(x, y - 1))
    return nx.shortest_path_length(
        cave, source=(0, 0), target=(m * size - 1, n * size - 1), weight="weight"
    )


print("Part 1:", solve())
print("Part 2:", solve(5))
