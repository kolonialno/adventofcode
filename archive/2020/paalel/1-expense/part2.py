from sys import exit
from part1 import value_in_array


def main():
    """
    Find the triplet (a,b,c): such that a + b + c = 2020

    Solved by sorting the array and binary searching for the value that completes
    the triplet.
    """

    with open('input.txt', 'r+') as file:
        array = sorted(int(line) for line in file.readlines())


    for a in array:
        for b in array:
            if value_in_array(array, 0, len(array)-1, 2020-a-b):
                print("a:", a, "b:", b, "c:", 2020-a-b, "a*b*c:",a*b*(2020-a-b))
                exit()

    

if __name__ == "__main__":
    main()
