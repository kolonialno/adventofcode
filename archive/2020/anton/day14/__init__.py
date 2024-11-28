from collections import defaultdict
from itertools import starmap


def load_instructions(filename):
    with open(filename) as in_file:
        lines = (x.split("=") for x in in_file)
        cleaned = ((x[0].strip(), x[1].strip()) for x in lines)
        commands = []
        for start, value in cleaned:
            if start.startswith("mask"):
                commands.append(("mask", value))
            else:
                commands.append(
                    (
                        "mem",
                        int(start.replace("[", "").replace("]", "")[3:]),
                        int(value),
                    )
                )
        return commands


def mask(x, y):
    return x if x in ("0", "1") else y


def mask_floating(x, y):
    return y if x == "0" else x


def generate_floating_addresses(masked, expanded=""):
    index = len(expanded)
    if len(masked) == index:
        return [
            expanded,
        ]
    if masked[index] == "X":
        return generate_floating_addresses(
            masked, expanded + "0"
        ) + generate_floating_addresses(masked, expanded + "1")
    else:
        return generate_floating_addresses(masked, expanded + masked[index])


class TrickymemoryBase:
    def __init__(self):
        self.memory = defaultdict(lambda x: 0)
        self.mask_register = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

    def sum_memory(self):
        return sum(self.memory.values())

    def mask(self, value):
        self.mask_register = value

    def execute(self, line):
        instruction = line[0]
        getattr(self, instruction)(*line[1:])


class MaskedAssignmentMemory(TrickymemoryBase):
    def mem(self, location, value):
        bin_val = format(value, "036b")
        masked = "".join(starmap(mask, zip(self.mask_register, bin_val)))
        self.memory[location] = int(masked, 2)


class MaskedAddressingMemory(TrickymemoryBase):
    def mem(self, location, value):
        bin_val = format(location, "036b")
        masked = "".join(starmap(mask_floating, zip(self.mask_register, bin_val)))
        for address in generate_floating_addresses(masked):
            self.memory[int(address, 2)] = value


def testcase() -> int:
    lines = load_instructions("inputs/14testcase.txt")
    mem = MaskedAssignmentMemory()
    for x in lines:
        mem.execute(x)
    return mem.sum_memory()


def testcase_generate_floating() -> int:
    mem = "00000000000000000000000000000001X0XX"
    return len(generate_floating_addresses(mem))


def main() -> int:
    lines = load_instructions("inputs/14.txt")
    mem = MaskedAssignmentMemory()
    for x in lines:
        mem.execute(x)
    return mem.sum_memory()


def testcase_secondary() -> int:
    lines = load_instructions("inputs/14testcase_secondary.txt")
    mem = MaskedAddressingMemory()
    for x in lines:
        mem.execute(x)
    return mem.sum_memory()


def secondary() -> int:
    lines = load_instructions("inputs/14.txt")
    mem = MaskedAddressingMemory()
    for x in lines:
        mem.execute(x)
    return mem.sum_memory()