import queue
from collections import defaultdict

def initialize_stacks():
    # Initialize stacks
    stacks = defaultdict(lambda: queue.LifoQueue())
    for input in reversed(STACKS_INPUT):
        for i, stack_num in enumerate(STACK_NUMBERS):
            if input[i] != '*':
                stacks[stack_num].put(input[i])
    return stacks

def move_crates(stacks, CrateMoverType = 9000):

    for instruction in INSTRUCTIONS:
        ins = instruction.split("move ")[1].split(" ")
        num_items = int(ins[0])
        from_stack = ins[2]
        to_stack = ins[4]
        if CrateMoverType == 9000:
            for i in range(num_items):
                crate_9000 = stacks[from_stack].get()
                stacks[to_stack].put(crate_9000)
        elif CrateMoverType == 9001:
            crate_9001 = queue.LifoQueue() # Used for CrateMover 9001
            for i in range(num_items):
                crate_9001.put(stacks[from_stack].get())
            for i in range(num_items):
                stacks[to_stack].put(crate_9001.get())

    top_crates = "".join([stacks[stack_num].get() for stack_num in STACK_NUMBERS])
    print(top_crates)

if __name__ == '__main__':

    with open("input.txt", "r") as f:
        data = f.read().split("\n\n")

        # Clean data
        stacks_raw, stack_numbers = data[0].rsplit("\n", 1)
        # [('*', 'D', '*'), ('N', 'C', '*'), ('Z', 'M', 'P')]
        STACKS_INPUT = []
        for row in stacks_raw.replace("    ", "[*]").replace(" ", "").split("\n"):
            STACKS_INPUT.append([el.replace("]", "")  for i, el in enumerate(row.split("[")) if i != 0])

        #['1', '2', '3']
        STACK_NUMBERS = stack_numbers.strip().replace("   ", " ").split(" ")

        INSTRUCTIONS = data[1].strip().split("\n")

    stacks = initialize_stacks()
    move_crates(stacks, CrateMoverType = 9000)
    stacks = initialize_stacks()
    move_crates(stacks, CrateMoverType = 9001)
