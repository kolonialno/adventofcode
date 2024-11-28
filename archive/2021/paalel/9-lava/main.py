import numpy as np


def is_low_point(matrix, i, j):
    (M, N) = matrix.shape

    if matrix[i,j] >= (matrix[i-1,j] if i > 0 else 10):
        return False
    if matrix[i,j] >= (matrix[i+1,j] if i < M-1 else 10):
        return False
    if matrix[i,j] >= (matrix[i,j-1] if j > 0 else 10):
        return False
    if matrix[i,j] >= (matrix[i,j+1] if j < N-1 else 10):
        return False

    return True

def find_low_points(matrix):
    (M, N) = matrix.shape
    low_points = []
    for i in range(M):
        for j in range(N):
            if is_low_point(matrix, i, j):
                low_points.append((i, j))

    return low_points

def calc_basin_size(matrix, i, j, visited):
    if matrix[i,j] == 9 or (i, j) in visited:
        return 0

    visited.append((i,j))
    (M, N) = matrix.shape
    return_value = 1
    if matrix[i,j] < (matrix[i-1,j] if i > 0 else -1):
        return_value += calc_basin_size(matrix, i-1, j, visited)
    if matrix[i,j] < (matrix[i+1,j] if i < M-1 else -1):
        return_value += calc_basin_size(matrix, i+1, j, visited)
    if matrix[i,j] < (matrix[i,j-1] if j > 0 else -1):
        return_value += calc_basin_size(matrix, i, j-1, visited)
    if matrix[i,j] < (matrix[i,j+1] if j < N-1 else -1):
        return_value += calc_basin_size(matrix, i, j+1, visited)

    return return_value


def solve_1(filename):
    with open(filename) as file:
        matrix = np.array([[n for n in line.strip()] for line in file.readlines()], dtype=int)
    low_points = find_low_points(matrix)

    return sum(matrix[i,j]+1 for (i,j) in low_points)

def solve_2(filename):
    with open(filename) as file:
        matrix = np.array([[n for n in line.strip()] for line in file.readlines()], dtype=int)

    low_points = find_low_points(matrix)
    basins = []

    for i,j in low_points:
        basins.append(calc_basin_size(matrix, i,j, []))

    return np.prod(sorted(basins)[-3:])

assert solve_1("test_input.txt") == 15
print("Solution part 1:", solve_1("input.txt"))

assert solve_2("test_input.txt") == 1134
print("Solution part 2:", solve_2("input.txt"))
