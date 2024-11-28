import numpy as np


def main():
    """
    Find k s.t i+j=k where i!=j, i,j in k_-25, ..,k
    """

    with open("input.txt", "r+") as file:
        array = np.array([int(line) for line in file.readlines()])

    # 1. Create matrix with combinations i+j, i,j in array
    # 2. limit to diagonals 1 -> 25
    # 3. Find first element of array that is not in the banded digonal matrix

    N = 25
    x = np.meshgrid(array, array)[0]
    y = x + x.T
    matrix = np.triu(y, k=1) - np.triu(y, k=N + 1)

    for i in range(N, array.shape[0]):
        if not array[i] in matrix:
            contiguous_number = array[i]
            print("contiguous number: ", contiguous_number)
            break


if __name__ == "__main__":
    main()
