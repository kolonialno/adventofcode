with open("input.txt") as f:
    data = [
        tuple([int(c) for c in l.strip().split(",")]) for l in f.read().splitlines()
    ]


def get_neighbouring_cubes(cube):

    neighbour_cubes = []
    for x, y, z in [
        (1, 0, 0),
        (-1, 0, 0),
        (0, 1, 0),
        (0, -1, 0),
        (0, 0, 1),
        (0, 0, -1),
    ]:
        neighbour_cube = tuple([cube[0] + x, cube[1] + y, cube[2] + z])
        neighbour_cubes.append(neighbour_cube)

    return neighbour_cubes


total_sides_exposed = 0
for cube in data:
    sides_covered = 0
    neighbour_cubes = get_neighbouring_cubes(cube)
    for neighbour_cube in neighbour_cubes:
        if neighbour_cube in data:
            sides_covered += 1
    total_sides_exposed += 6 - sides_covered


print(total_sides_exposed)

starting_point = (0, 0, 0)
cubes_to_visit = set([starting_point])
visited_cubes = set()

total_exposed_sides = 0
while len(cubes_to_visit) > 0:
    cube = cubes_to_visit.pop()
    visited_cubes.add(cube)
    neighbour_cubes = get_neighbouring_cubes(cube)
    for neighbour_cube in neighbour_cubes:
        if max(neighbour_cube) <= 21 and min(neighbour_cube) >= -1:
            if neighbour_cube in data:
                total_exposed_sides += 1
            elif not neighbour_cube in visited_cubes:
                cubes_to_visit.add(neighbour_cube)

print(total_exposed_sides)
