import numpy as np

def read_input(filename):
    with open(filename, "r+") as file:
        lines= []
        for line in file.readlines():
            points = line.strip().replace(" ", "").split("->")
            lines.append([
                tuple(int(p) for p in points[0].split(",")),
                tuple(int(p) for p in points[1].split(","))
            ])
        return lines

def create_grid(lines, N, include_diagonals=False):
    grid = np.zeros((N,N))
    for line in lines:
        x0, y0 = line[0]
        x1, y1 = line[1]

        slope_y = -1 if y1 < y0 else 1
        slope_x = -1 if x1 < x0  else 1
        if x0 == x1:
            for d in np.arange(y0, y1 + slope_y, slope_y):
                grid[d, x0] += 1
        elif y0 == y1:
            for d in np.arange(x0, x1 + slope_x, slope_x):
                grid[y0, d] += 1
        elif include_diagonals:
            for d in np.arange(0, abs(x0-x1) + 1):
                grid[y0 + slope_y*d, x0+slope_x*d] += 1
    return grid
        

def count_overlaps(grid, limit):
    overlaps = 0
    N = len(grid)
    for i in range(N):
        for j in range(N):
            overlaps += 1 if grid[i,j] >= limit else 0

    return overlaps

def part1(lines, N=10):
    grid = create_grid(lines, N)
    return count_overlaps(grid, limit=2)

def part2(lines, N=10):
    grid = create_grid(lines, N, include_diagonals=True)
    return count_overlaps(grid, limit=2)


if __name__ == "__main__":
    test_lines = read_input("test_input.txt")
    assert part1(test_lines) == 5

    lines = read_input("input.txt")
    print("Solution part 1:", part1(lines, N=1000))

    test_lines = read_input("test_input.txt")
    assert part2(test_lines) == 12

    lines = read_input("input.txt")
    print("Solution part 2:", part2(lines, N=1000))
