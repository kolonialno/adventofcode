from sys import stdin
from collections import deque, defaultdict

input_sequence = [int(x) for x in next(stdin).strip().split(",")]


def play(input_sequence, final_spoken):
    initial_numbers_spoken = len(input_sequence)
    memory = defaultdict(lambda: deque([], maxlen=2))

    for i, number in enumerate(input_sequence, start=1):
        memory[number].append(i)

    last_spoken = input_sequence[-1]

    for i in range(initial_numbers_spoken + 1, final_spoken + 1):
        if len(memory[last_spoken]) == 1:
            last_spoken = 0
            memory[last_spoken].append(i)
        elif len(memory[last_spoken]) == 2:
            last_spoken = memory[last_spoken][-1] - memory[last_spoken][-2]
            memory[last_spoken].append(i)

    return last_spoken


def a():
    return play(input_sequence, 2020)


def b():
    return play(input_sequence, 30000000)


print(a())
print(b())
