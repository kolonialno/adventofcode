from sys import stdin
import re

lines = [line.strip() for line in stdin]

INSTRUCTION_REGEX = re.compile(r"^([\w]{3}) ([+-])(\d+)$")


def parse_instruction(instruction):
    match = INSTRUCTION_REGEX.search(instruction)
    if match:
        (opcode, sign, value) = match.groups()
        factor = -1 if sign == "-" else 1
        return (opcode, factor * int(value))
    raise ValueError()


instructions = [parse_instruction(instruction) for instruction in lines]


def run(program):
    used = set()
    accumulator = 0

    pc = 0
    while pc < len(program):
        (opcode, value) = program[pc]
        if pc in used:
            return (accumulator, False)

        used.add(pc)

        if opcode == "acc":
            accumulator += value
            pc += 1
        elif opcode == "nop":
            pc += 1
        elif opcode == "jmp":
            pc += value

    return (accumulator, True)


def a():
    (accumulator, _) = run(instructions)
    return accumulator


def b():
    permutations = []

    replaceable = {
        (i, instruction)
        for (i, instruction) in enumerate(instructions)
        if instruction[0] == "nop" or instruction[0] == "jmp"
    }

    for (i, instruction) in replaceable:
        copy = instructions.copy()
        if instruction[0] == "jmp":
            copy[i] = ("nop", instruction[1])
        elif instruction[0] == "nop":
            copy[i] = ("jmp", instruction[1])

        permutations.append(copy)

    for program in permutations:
        (accumulator, terminates) = run(program)

        if terminates:
            return accumulator

    return "not-solved"


print(a())
print(b())