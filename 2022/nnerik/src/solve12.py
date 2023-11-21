import networkx as nx


def build_graph(data):
    lines = data.splitlines()
    graph = nx.DiGraph()
    starts = []
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            match char:
                case "S":
                    start = (x, y)
                    height = 0
                case "E":
                    end = (x, y)
                    height = 25
                case _:
                    height = ord(char) - ord("a")
            graph.add_node((x, y), height=height)
            if height == 0:
                starts.append((x, y))
            if y > 0:
                height_north = graph.nodes[(x, y - 1)]["height"]
                if height_north <= height + 1:
                    graph.add_edge((x, y), (x, y - 1))
                if height <= height_north + 1:
                    graph.add_edge((x, y - 1), (x, y))
            if x > 0:
                height_west = graph.nodes[(x - 1, y)]["height"]
                if height_west <= height + 1:
                    graph.add_edge((x, y), (x - 1, y))
                if height <= height_west + 1:
                    graph.add_edge((x - 1, y), (x, y))
    return graph, starts, start, end


def shortest_path(graph, starts, end):
    for start in starts:
        try:
            length = nx.shortest_path_length(graph, start, end)
        except nx.NetworkXNoPath:
            continue
        yield length


def solve1(data):
    graph, _, start, end = build_graph(data)
    return next(shortest_path(graph, [start], end))


def solve2(data):
    graph, starts, _, end = build_graph(data)
    return min(shortest_path(graph, starts, end))
