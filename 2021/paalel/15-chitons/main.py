import numpy as np
import heapq as hq

def get_neigbhours(matrix, point):
    N, M = matrix.shape
    neigbhours = []
    if point[0] > 0:
        neigbhours.append((point[0] - 1, point[1]))
    if point[0] < N - 1:
        neigbhours.append((point[0] + 1, point[1]))
    if point[1] > 0:
        neigbhours.append((point[0], point[1] - 1))
    if point[1] < M - 1:
        neigbhours.append((point[0], point[1] + 1))

    return neigbhours

def dijkstra(matrix, target):
    """
    pseudo code shamelessly stolen from wikipedia
    """

    dist = np.full(matrix.shape, np.inf)
    prev = np.full(matrix.shape, None)

    dist[0,0] = 0
    queue = []
    for p in get_neigbhours(matrix, (0,0)):
        hq.heappush(queue, (matrix[p], p))
        dist[p] = matrix[p]

    while queue:
        u = hq.heappop(queue)

        for v in get_neigbhours(matrix, u[1]):
            alt = u[0] + matrix[v]
            if alt <  dist[v]:
                prev[v] = u[1]
                dist[v] = alt
                hq.heappush(queue, (alt, v))

    path = [target]
    while True:
        previous = prev[path[-1]]
        if previous is None:
            break
        path.append(previous)

    return path


def solve(filename, tiling=False):
    with open(filename) as file:
        matrix = np.array([[n for n in line.strip()] for line in file.readlines()], dtype=int)

    if tiling:
        n, m = matrix.shape
        matrix = np.tile(matrix,(5,5))
        for j in range(5):
            for i in range(5):
                if j == 0 and i == 0:
                    continue
                if j == 0:
                    matrix[i*n:(i+1)*n, 0:m] = matrix[(i-1)*n:i*n, 0:m] % 9 + 1
                    continue

                matrix[i*n:(i+1)*n, j*m:(j+1)*m] = matrix[i*n:(i+1)*n,(j-1)*m:j*m] % 9 + 1

    N, M = matrix.shape
    return sum([matrix[p[0], p[1]] for p in dijkstra(matrix, (N-1,M-1))])


assert solve("test_input.txt") == 40
print("Solution problem 1: ", solve("input.txt"))

assert solve("test_input.txt", tiling=True) == 315
print("Solution problem 2: ", solve("input.txt", tiling=True))
