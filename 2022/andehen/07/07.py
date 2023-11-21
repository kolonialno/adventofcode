class File:
    def __init__(self, name, size) -> None:
        self.name = name
        self.size = size


class Dir:
    def __init__(self, name, parent=None) -> None:
        self.name = name
        self.dirs = []
        self.files = []
        self.parent = parent

    def get_size(self):
        return sum([f.size for f in self.files]) + sum(
            [d.get_size() for d in self.dirs]
        )

    def has_dir(self, name):
        return name in [d.name for d in self.dirs]

    def get_dir(self, name):
        return [d for d in self.dirs if d.name == name][0]

    def get_all_dir_sizes(self) -> list:
        dir_sizes = []
        for d in self.dirs:
            dir_sizes += d.get_all_dir_sizes()
        dir_sizes.append((self.name, self.get_size()))
        return dir_sizes


def cd(current_dir: Dir, arg: str):
    if arg == "/":
        return root
    if arg == ".." and current_dir.parent:
        return current_dir.parent

    if not current_dir.has_dir(arg):
        return current_dir

    return current_dir.get_dir(arg)


root = Dir("/")
current_dir = root

with open("input.txt") as f:
    for terminal_line in f.readlines():
        if terminal_line.startswith("$ "):
            if terminal_line.startswith("$ cd"):
                cmd, arg = terminal_line[2:].strip().split(" ")
                current_dir = cd(current_dir, arg)
        else:
            arg, name = terminal_line.strip().split(" ")
            if arg == "dir":
                current_dir.dirs.append(Dir(name, current_dir))
            else:
                current_dir.files.append(File(name, int(arg)))

dir_sizes = root.get_all_dir_sizes()

print(sum([size for _, size in dir_sizes if size <= 100000]))

disk_size = 70000000
required_free_space = 30000000
unused_space = disk_size - root.get_size()
need_to_delete = required_free_space - unused_space

print(sorted([size for _, size in dir_sizes if size >= need_to_delete])[0])
