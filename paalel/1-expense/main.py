def value_in_array(array, low, high, value):
    """
    Binary search for the value.
    """

    if high < low:
        return False

    mid = (high + low) // 2

    if array[mid] == value:
        return True

    elif array[mid] > value:
        return value_in_array(array, low, mid-1, value)

    elif array[mid] < value:
        return value_in_array(array, mid+1, high, value)
    
    

def main():
    """
    Find the pair of values such that a + b = 2020

    Solved by sorting the array and binary searching for the value that completes
    the pair.
    """

    with open('input.txt', 'r+') as file:
        array = sorted([int(line) for line in file.readlines()])


    for number in array:
        if value_in_array(array, 0, len(array)-1, 2020-number):
            print("a:", number, "b:", 2020-number, "a*b:",number*(2020-number))
            break

    

if __name__ == "__main__":
    main()
