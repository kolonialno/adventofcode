from functools import reduce


def read_input(filename):
    with open(filename) as f:
        numbers = [int(n.strip()) for n in f.readline().split(",")]
        grids = []
        while f.readline():
            rows = []
            for _ in range(5):
                line = f.readline()
                rows.append([int(line[i : i + 3].strip()) for i in range(0, 15, 3)])
            grids.append(rows)
        return numbers, grids


def prepare_bards(grids):
    boards = []
    lines = []
    marks = []
    for grid in grids:
        boards.append(reduce(lambda a, b: a.union(b), grid, set()))
        grid_lines = list(map(set, grid))
        grid_lines.extend(map(set, zip(*grid)))
        lines.append(grid_lines)
        marks.append([0 for _ in range(10)])
    return boards, lines, marks


def part1(input):
    numbers, grids = input
    boards, lines, marks = prepare_bards(grids)
    for n in numbers:
        for i, board in enumerate(boards):
            if n in board:
                board.remove(n)
                for j, line in enumerate(lines[i]):
                    if n in line:
                        marks[i][j] += 1
                        if marks[i][j] == 5:
                            return reduce(lambda a, b: a + b, board) * n


def part2(input):
    numbers, grids = input
    numbers, grids = input
    boards, lines, marks = prepare_bards(grids)
    for n in numbers:
        winners = []
        for i, board in enumerate(boards):
            if n in board:
                board.remove(n)
                for j, line in enumerate(lines[i]):
                    if n in line:
                        marks[i][j] += 1
                        if marks[i][j] == 5:
                            if len(boards) == 1:
                                return reduce(lambda a, b: a + b, board) * n
                            winners.append(i)
                            break
        for i in reversed(winners):
            boards.pop(i)
            lines.pop(i)
            marks.pop(i)


if __name__ == "__main__":
    input = read_input(__file__[:-3] + ".txt")
    print(f"Part 1: {part1(input)}")
    print(f"Part 2: {part2(input)}")
