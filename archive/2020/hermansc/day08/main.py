instructions = [(ins.split()[0], int(ins.split()[1])) for ins in open("program.txt").read().strip().split("\n")]

def run_program(program):
    accumulator = 0
    visited = set()
    idx = 0
    while True:
        if idx in visited:
            return False, accumulator
        visited.add(idx)

        operation, argument = program[idx]

        if operation == "nop":
            idx += 1
        elif operation == "acc":
            accumulator += argument
            idx += 1
        elif operation == "jmp":
            idx += argument

        # Terminated successfully
        if idx >= len(program):
            return True, accumulator

# Part 1
print(f"1: Accumulator upon entering infinite loop: {run_program(instructions)[1]}")

# Part 2
for idx in range(len(instructions)):
    op, arg = instructions[idx]
    if not op in ("jmp", "nop"):
        continue

    modified_instructions = instructions.copy()
    new_op = "jmp" if op == "nop" else "nop"
    modified_instructions[idx] = (new_op, arg)

    terminated, accumulator = run_program(modified_instructions)
    if terminated:
        print(f"2: Terminated! Changed {op} to {new_op} on instruction {idx}. Accumulator: {accumulator}")
