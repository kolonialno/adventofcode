from __future__ import annotations

from pathlib import Path
from typing import TypeAlias

import networkx as nx

Pos: TypeAlias = tuple[int, int]


def solve_a(data: str) -> int:
    heightmap, start, end = parse(data)
    graph = get_graph(heightmap)
    return nx.shortest_path_length(graph, start, end)


def solve_b(data: str) -> int:
    heightmap, _start, end = parse(data)
    graph = get_graph(heightmap)
    lowland = [pos for pos, h in heightmap.items() if h == 1]
    return min(
        nx.shortest_path_length(graph, start, end)
        for start in lowland
        if nx.has_path(graph, start, end)
    )


def parse(data: str) -> tuple[dict[Pos, int], Pos, Pos]:
    heightmap: dict[Pos, int] = {}
    start: Pos | None = None
    end: Pos | None = None
    for y, line in enumerate(data.splitlines()):
        for x, value in enumerate(line):
            if value == "S":
                start = (x, y)
                value = "a"
            elif value == "E":
                end = (x, y)
                value = "z"
            heightmap[(x, y)] = ord(value) - ord("a") + 1
    assert start and end
    return (heightmap, start, end)


def get_graph(heights: dict[Pos, int]) -> nx.DiGraph:
    graph = nx.DiGraph()
    for (x, y), h in heights.items():
        if (v := heights.get((x + 1, y))) and v <= h + 1:
            graph.add_edge((x, y), (x + 1, y))
        if (v := heights.get((x - 1, y))) and v <= h + 1:
            graph.add_edge((x, y), (x - 1, y))
        if (v := heights.get((x, y + 1))) and v <= h + 1:
            graph.add_edge((x, y), (x, y + 1))
        if (v := heights.get((x, y - 1))) and v <= h + 1:
            graph.add_edge((x, y), (x, y - 1))
    return graph


def test() -> None:
    data = Path("test12.txt").read_text()
    assert solve_a(data) == 31
    assert solve_b(data) == 29


if __name__ == "__main__":
    data = Path("input12.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
