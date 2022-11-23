import numpy as np
import re

def parse_input(input_file: str):
    with open(input_file) as fh:
        input = fh.readlines()

    input = [int(i) for i in input[0].split(',')]

    return input


def solve(input_file: str, debug: bool = False):
    input = parse_input(input_file)
    if debug:
        print(input)

    num_states = 9
    state = np.bincount(input, minlength=9)

    num_days = 256
    for i in range(num_days):
        new_state = np.zeros_like(state)
        new_state[:num_states-1] = state[1:num_states]
        new_state[6] += state[0]
        new_state[8] += state[0]
        if debug:
            print(i, new_state)
        state = new_state

        if i == 17:
            print(f"After 18 days: {state.sum()}")

    return state.sum()

print(solve("test_input.txt", debug=False))

print(solve("input.txt", debug=False))
