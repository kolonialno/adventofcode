current = []
visited = []


with open("input.txt") as f:
    for terminal_line in f.readlines():
        if terminal_line.startswith("$ "):
            if terminal_line.startswith("$ cd"):
                cmd, arg = terminal_line[2:].strip().split(" ")
                if arg == "..":
                    visited.append(current.pop())
                else:
                    current.append(0)
        else:
            arg, name = terminal_line.strip().split(" ")
            if arg.isdigit():
                current = [d + int(arg) for d in current]

all = current + visited
print(sum([s for s in all if s <= 100_000]))

need_to_delete = 30_000_000 - (70_000_000 - all[0])
print(sorted([s for s in all if s >= need_to_delete])[0])
