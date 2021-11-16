def create_matrix():
    with open("input.txt", "r+") as file:
        return [
            [1 if char == "#" else 0 for char in line.rstrip()]
            for line in file.readlines()
        ]


def main():
    """
    Find the number of trees hit for each trajectory and multiply the result.
    """
    config = [[1, 1], [1, 3], [1, 5], [1, 7], [2, 1]]
    matrix = create_matrix()
    height, width = len(matrix), len(matrix[0])
    result = 1

    for down, right in config:
        col = 0
        num_trees = 0
        for row in range(0, height, down):
            num_trees += matrix[row][col % width]
            col += right

        result *= num_trees
        print(down, right, ": ", num_trees)

    print("result:", result)


if __name__ == "__main__":
    main()
