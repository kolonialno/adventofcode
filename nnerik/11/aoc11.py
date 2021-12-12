with open("input.txt", "r") as f:
    state = [int(n) for line in f for n in line.strip()]
    count = step = 0
    while sum(state):
        step += 1
        state = [s + 1 for s in state]
        while flashes := sum(state[i] > 9 for i in range(100)):
            count += flashes if step <= 100 else 0
            influx = [
                sum(
                    state[i + dx + dy] > 9
                    for dx in (-1, 0, 1)
                    for dy in (-10, 0, 10)
                    if 0 <= i + dy < 100 and 0 <= i % 10 + dx < 10
                )
                for i in range(100)
            ]
            state = [s + delta if 0 < s < 10 else 0 for s, delta in zip(state, influx)]
    print("Part 1:", count)
    print("Part 2:", step)
