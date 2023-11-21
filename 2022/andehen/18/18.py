from typing_extensions import TypeAlias
from operator import add


Cube: TypeAlias = tuple[int, int, int]


RELATIVE_NEIGHBOUR_CUBES = [
    (1, 0, 0),
    (-1, 0, 0),
    (0, 1, 0),
    (0, -1, 0),
    (0, 0, 1),
    (0, 0, -1),
]

LOWER_BOUND = -1
UPPER_BOUND = 21


def get_neighbouring_cubes(cube: Cube) -> list[Cube]:
    return [
        c
        for c in [tuple(map(add, cube, rnc)) for rnc in RELATIVE_NEIGHBOUR_CUBES]
        if (max(c) <= UPPER_BOUND and min(c) >= LOWER_BOUND)
    ]


def get_sides_covered(cubes: set[Cube], cube: Cube) -> int:
    return sum([1 for nc in get_neighbouring_cubes(cube) if nc in cubes])


def get_total_sides_exposed(cubes: set[Cube]) -> int:
    return sum([6 - get_sides_covered(cubes, cube) for cube in cubes])


def get_total_sides_exposed_to_air(cubes: set[Cube]) -> int:

    starting_point: Cube = (0, 0, 0)
    cubes_to_visit: set[Cube] = set([starting_point])
    cubes_visited: set[Cube] = set()

    total_sides_exposed_to_air = 0
    while len(cubes_to_visit) > 0:
        cube = cubes_to_visit.pop()
        cubes_visited.add(cube)
        neighbour_cubes = get_neighbouring_cubes(cube)
        total_sides_exposed_to_air += sum(
            [
                1
                if (nc in cubes)
                else (0 if nc in cubes_visited else int(bool(cubes_to_visit.add(nc))))
                for nc in neighbour_cubes
            ]
        )
    return total_sides_exposed_to_air


with open("input.txt") as f:
    cubes: set[Cube] = set(
        [tuple([int(c) for c in l.strip().split(",")]) for l in f.read().splitlines()]
    )

print(get_total_sides_exposed(cubes))
print(get_total_sides_exposed_to_air(cubes))
