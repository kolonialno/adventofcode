import re
from copy import deepcopy
from sys import stdin

REGEX = re.compile(r"^move (\d+) from (\d+) to (\d+)")


def parse_instruction(line: str) -> tuple[int, int, int]:
    match = REGEX.search(line)
    assert match

    n, source, destination = map(int, match.groups())
    return n, source, destination


def create_input_parser():
    stacks = []
    for line in stdin:
        if line.startswith(" 1"):
            break

        j = 0
        for i in range(0, len(line) - 1, 4):
            if len(stacks) <= j:
                stacks.append([])

            if line[i] != " ":
                stacks[j].append(line[i + 1])

            j += 1

    next(stdin)

    instructions = [parse_instruction(line.strip()) for line in stdin]

    def _get():
        return deepcopy(stacks), instructions

    return _get


get_input = create_input_parser()


def a():
    stacks, instructions = get_input()
    for n, src, dest in instructions:
        for _ in range(n):
            moved = stacks[src - 1].pop(0)
            stacks[dest - 1].insert(0, moved)

    return "".join(stack[0] for stack in stacks)


def b():
    stacks, instructions = get_input()
    for n, src, dest in instructions:
        moved = stacks[src - 1][:n]
        stacks[src - 1] = stacks[src - 1][n:]
        stacks[dest - 1] = moved + stacks[dest - 1]

    return "".join(stack[0] for stack in stacks)


print(a())
print(b())
