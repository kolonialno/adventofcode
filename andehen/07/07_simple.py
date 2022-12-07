current = []
visited = []

with open("input.txt") as f:
    for line in f.readlines():
        match line.split():
            case ["$", "cd", ".."]:
                visited.append(current.pop())
            case ["$", "cd", arg]:
                current.append(0)
            case [size, name] if size.isdigit():
                current = [d + int(size) for d in current]

all = current + visited
print(sum([size for size in all if size <= 100_000]))

need_to_delete = 30_000_000 - (70_000_000 - all[0])
print(sorted([size for size in all if size >= need_to_delete])[0])
