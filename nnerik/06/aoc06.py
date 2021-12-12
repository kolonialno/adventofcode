def solve(state, steps):
    for _ in range(steps):
        state = state[1:] + [state[0]]
        state[6] += state[-1]
    return sum(state)


state = [0] * 9
with open("input.txt") as f:
    for i in f.readline().split(","):
        state[int(i)] += 1

print("Part 1:", solve(state, 80))
print("Part 2:", solve(state, 256))
