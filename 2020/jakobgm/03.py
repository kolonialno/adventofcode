from pathlib import Path

INPUT_DIR = Path(__file__).parent / "input"

if __name__ == "__main__":
    input_path = INPUT_DIR / "day03.txt"
    values = {".": 0, "#": 1}
    rows = input_path.read_text().strip().split("\n")
    matrix = []
    for row in rows:
        matrix.append([values[column] for column in row])

    def solve(step):
        position = [0, 0]
        trees = 0
        num_rows = len(matrix)
        num_columns = len(matrix[0])
        while position[0] < num_rows - 1:
            position[0] += step[0]
            position[1] += step[1]
            trees += matrix[position[0]][position[1] % num_columns]
        return trees

    print("03a:", solve(step=(1, 3)))

    steps = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
    costs = [solve(step) for step in steps]
    print("03b:", costs[0] * costs[1] * costs[2] * costs[3] * costs[4])
