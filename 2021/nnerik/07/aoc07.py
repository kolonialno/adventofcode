def solve(crabs, cost):
    return min(
        sum(cost(abs(c - i)) for c in crabs) for i in range(min(crabs), max(crabs) + 1)
    )


with open("input.txt") as f:
    input = [int(n) for n in f.readline().split(",")]

print("Part 1:", solve(input, lambda a: a))
print("Part 2:", solve(input, lambda a: a * (a + 1) // 2))
