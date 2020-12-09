numbers = [int(line) for line in open("xmas.txt").read().strip().split("\n")]

lookback = 0
preamble = 25

for idx in range(preamble, len(numbers)):
    is_product = any(numbers[idx] == x for x in [x+y for x in numbers[lookback:idx] for y in numbers[lookback:idx]])

    if lookback < idx - preamble:
        lookback += 1

    if is_product:
        continue

    # Part 1
    print(numbers[idx])

    # Part 2
    for curr in range(idx):
        for lookahead in range(idx):
            combo = numbers[curr:lookahead]
            if numbers[idx] == sum(numbers[curr:lookahead]):
                print(min(combo)+max(combo))
                break
