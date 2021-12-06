def read_input(filename):
    initial =[0] * 9
    with open(filename, "r+") as file:
        line = file.readline()
        for n in line.rstrip().split(","):
            initial[int(n)] += 1

        return initial

def solve_jellies(jellies, N=10):
    for _ in range(N):
        jellies.insert(8, jellies.pop(0))
        jellies[6] += jellies[8]

    return sum(jellies)


if __name__ == "__main__":
    test_inital = read_input("test_input.txt")
    assert solve_jellies(test_inital, N=18) == 26
    test_inital = read_input("test_input.txt")
    assert solve_jellies(test_inital, N=80) == 5934

    inital = read_input("input.txt")
    print("Solution part 1:", solve_jellies(inital, N=80))

    test_lines = read_input("test_input.txt")
    assert solve_jellies(test_lines, N=256) == 26984457539

    lines = read_input("input.txt")
    print("Solution part 2:", solve_jellies(lines, N=256))
