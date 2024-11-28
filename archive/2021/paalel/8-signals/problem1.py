
def solve(filename):
    return_value =  0
    with open(filename) as file:
        for line in file.readlines():
            for s in line.strip().split("|")[-1].split(" "):
                return_value += 1 if len(s) in [2,4,3,7] else 0

    return return_value

assert solve("test_input.txt") == 26
print("solution to problem 1", solve("input.txt"))
