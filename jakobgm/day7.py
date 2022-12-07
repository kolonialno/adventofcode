from pathlib import Path

problem = Path("./input/7.txt").read_text().strip()

cwd = Path("/")
directories = {cwd}
files: dict[Path, int] = {}
for session in problem.split("$ ")[1:]:
    execution, output = session.split("\n", 1)
    command, *args = execution.split()
    if command == "cd":
        cwd = (cwd / args[0]).resolve()
        continue

    for line in output.splitlines():
        size, name = line.split(" ")
        if size == "dir":
            directories.add(cwd / name)
        else:
            files[cwd / name] = int(size)

# Task 1
sum = 0
directory_sizes: dict[Path, int] = {}
for directory in directories:
    directory_size = 0
    for file, file_size in files.items():
        if str(file.parent).startswith(str(directory)):
            directory_size += file_size

    directory_sizes[directory] = directory_size
    if directory_size <= 100_000:
        sum += directory_size

# Task 2
empty_space = 70_000_000 - directory_sizes[Path("/")]
required = 30000000 - empty_space
to_be_deleted = min(size for size in directory_sizes.values() if size >= required)

print(f"Task 1: {sum}")
print(f"Task 2: {to_be_deleted}")
