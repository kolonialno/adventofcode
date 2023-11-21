from collections import defaultdict


def get_dir_sizes(data):
    dir_totals = defaultdict(int)
    for input in data.split("$ ")[1:]:
        lines = input.strip().split("\n")
        cmd = lines[0].split(" ")
        if cmd[0] == "cd":
            if cmd[1] == "/":
                dir = ()
            elif cmd[1] == "..":
                dir = dir[:-1]
            else:
                dir = dir + (cmd[1],)
        else:
            for line in lines[1:]:
                size, _ = line.split(" ")
                if size != "dir":
                    for i in range(len(dir) + 1):
                        dir_totals[dir[:i]] += int(size)
    return dir_totals.values()


def solve1(data):
    return sum(size for size in get_dir_sizes(data) if size <= 100000)


def solve2(data):
    dir_sizes = sorted(get_dir_sizes(data))
    for size in dir_sizes:
        if size >= dir_sizes[-1] - 40000000:
            return size
