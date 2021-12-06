def solve_jellies(filename, N=10):
    with open(filename, "r+") as file:
        jellies =[0] * 9
        for n in file.readline().rstrip().split(","):
            jellies[int(n)] += 1

    for _ in range(N):
        jellies.insert(8, jellies.pop(0))
        jellies[6] += jellies[8]

    return sum(jellies)


assert solve_jellies("test_input.txt", N=18) == 26
assert solve_jellies("test_input.txt", N=80) == 5934
assert solve_jellies("test_input.txt", N=256) == 26984457539

print("Solution part 1:", solve_jellies("input.txt", N=80))
print("Solution part 2:", solve_jellies("input.txt", N=256))
