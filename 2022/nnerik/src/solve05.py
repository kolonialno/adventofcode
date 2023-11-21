def parse_inputs(data):
    stack_input, moves_input = data.split("\n\n")
    number_of_stacks = int(stack_input[-2])

    stacks = [[] for _ in range(number_of_stacks)]
    for line in stack_input.splitlines()[-2::-1]:
        for i in range(number_of_stacks):
            if (crate := line[i * 4 + 1]) != " ":
                stacks[i].append(crate)

    moves = []
    for line in moves_input.splitlines():
        moves.append(tuple(int(line.split()[i]) for i in (1, 3, 5)))

    return stacks, moves


def solve1(data):
    stacks, moves = parse_inputs(data)
    for n, s1, s2 in moves:
        for _ in range(n):
            stacks[s2 - 1].append(stacks[s1 - 1].pop())
    return "".join(s[-1] for s in stacks)


def solve2(data):
    stacks, moves = parse_inputs(data)
    for n, s1, s2 in moves:
        stacks[s2 - 1].extend(stacks[s1 - 1][-n:])
        stacks[s1 - 1] = stacks[s1 - 1][:-n]
    return "".join(s[-1] for s in stacks)
