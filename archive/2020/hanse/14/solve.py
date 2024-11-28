from sys import stdin
from collections import deque
import re

REGEX = re.compile(r"^mem\[(\d+)\] = (\d+)$")


writes = []
current_mask = None

for line in stdin:
    if line.startswith("mask"):
        current_mask = line.split("=")[1].strip()
        continue

    match = REGEX.search(line)
    if not match:
        raise ValueError()

    (address, value) = [int(g) for g in match.groups()]
    writes.append((address, value, current_mask))


def apply_value_mask(value, mask):
    bit_string = list(bin(value)[2:].rjust(36, "0"))
    for i in range(36):
        if mask[i] != "X":
            bit_string[i] = mask[i]
    return int("".join(bit_string), 2)


def apply_address_mask(address, mask):
    bit_string = list(bin(address)[2:].rjust(36, "0"))

    for i in range(36):
        if mask[i] != "0":
            bit_string[i] = mask[i]

    address_permutations = []
    queue = deque([bit_string])

    while queue:
        current_address = queue.popleft()
        try:
            x = current_address.index("X")
            for bit in ("0", "1"):
                copy = current_address[:]
                copy[x] = bit
                queue.append(copy)
        except ValueError:
            address_permutations.append(current_address)

    return [int("".join(bit_string), 2) for bit_string in address_permutations]


def a():
    memory = {}
    for (address, value, mask) in writes:
        memory[address] = apply_value_mask(value, mask)

    return sum(memory.values())


def b():
    memory = {}
    for (address, value, mask) in writes:
        for masked_adress in apply_address_mask(address, mask):
            memory[masked_adress] = value

    return sum(memory.values())


print(a())
print(b())
