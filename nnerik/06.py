def read_input(filename):
    state = [0] * 9
    with open(filename) as f:
        for i in f.readline().split(","):
            state[int(i)] += 1
    return state


def solve(state, steps):
    for _ in range(steps):
        state = state[1:] + [state[0]]
        state[6] += state[-1]
    return sum(state)


if __name__ == "__main__":
    initial = read_input(__file__[:-3] + ".txt")

    # Part 1
    result = solve(initial, 80)
    print(f"Part 1: {result}")

    # Part 2
    result = solve(initial, 256)
    print(f"Part 2: {result}")
