from math import prod


def parse_matrix(filename) -> list:
    with open(filename) as in_file:
        data = in_file.read()
    matrix = [list(item) for item in data.split("\n")]
    return matrix


def toboggan_traverse_matrix(matrix, h_offset, v_offset) -> list:
    hit_fields = []
    h_start = v_start = 0
    for row in matrix[v_start::v_offset]:
        hit_fields.append(row[h_start])
        h_start = (h_start + h_offset) % len(row)

    return hit_fields


def main() -> int:
    squares_hit = toboggan_traverse_matrix(parse_matrix("inputs/03.txt"), 3, 1)
    num_trees = squares_hit.count("#")
    return num_trees


def secondary() -> int:
    traversals = (
        (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2),
    )
    matrix = parse_matrix("inputs/03.txt")
    return prod([toboggan_traverse_matrix(matrix, *x).count("#") for x in traversals])


def testcase() -> int:
    squares_hit = toboggan_traverse_matrix(parse_matrix("inputs/03testcase.txt"), 3, 1)
    num_trees = squares_hit.count("#")
    return num_trees
