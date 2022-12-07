from __future__ import annotations

from pathlib import Path
from typing import TypeAlias

Dir: TypeAlias = dict[Path, "Entry"]
Entry: TypeAlias = Dir | int


def parse(data: str) -> Dir:
    root_path = Path("/")
    root_dir: Dir = {}

    stack = [(root_path, root_dir)]

    lines = iter(data.splitlines())
    for line in lines:
        curr_path, curr_dir = stack[-1]
        if line.startswith("$"):
            match line.split():
                case ["$", "cd", "/"]:
                    stack = [(root_path, root_dir)]
                case ["$", "cd", ".."]:
                    stack.pop()
                case ["$", "cd", name]:
                    new_path = curr_path / name
                    new_dir = curr_dir[new_path]
                    assert isinstance(new_dir, dict)
                    stack.append((new_path, new_dir))
                case ["$", "ls"]:
                    pass
                case cmd:
                    raise Exception(f"Command not found: {cmd}")
        else:
            match line.split():
                case ["dir", name]:
                    curr_dir[curr_path / name] = {}
                case [size, name]:
                    curr_dir[curr_path / name] = int(size)
                case other:
                    raise Exception(f"Unknown pattern: {other}")

    return root_dir


def get_dir_sizes(root_dir: Dir) -> dict[Path, int]:
    result: dict[Path, int] = {}

    def get_dir_size(dir: Dir) -> int:
        size = 0
        for path, entry in dir.items():
            if isinstance(entry, dict):
                result[path] = get_dir_size(entry)
                size += result[path]
            elif isinstance(entry, int):
                size += entry
        return size

    result[Path("/")] = get_dir_size(root_dir)
    return result


def solve_a(data: str) -> int:
    root_dir = parse(data)
    dir_sizes = get_dir_sizes(root_dir)

    sum = 0
    for dir_size in dir_sizes.values():
        if dir_size <= 100_000:
            sum += dir_size
    return sum


def solve_b(data: str) -> int:
    root_dir = parse(data)
    dir_sizes = get_dir_sizes(root_dir)

    disk_size = 70_000_000
    needed_free_size = 30_000_000
    free_size = disk_size - dir_sizes[Path("/")]
    to_free_size = needed_free_size - free_size

    for size in sorted(dir_sizes.values()):
        if size >= to_free_size:
            return size

    raise Exception("No large enough dir found")


def test() -> None:
    data = Path("test07.txt").read_text()
    assert solve_a(data) == 95437
    assert solve_b(data) == 24933642


if __name__ == "__main__":
    data = Path("input07.txt").read_text()
    print(solve_a(data))
    print(solve_b(data))
