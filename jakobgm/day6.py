from pathlib import Path


def buffer_start(signal: str, size: int) -> int:
    for index, chunk in enumerate(zip(*(signal[start:] for start in range(size)))):
        if len(set(chunk)) == len(chunk):
            return index + size


problem = Path("./input/6.txt").read_text().strip()
print(buffer_start(problem, size=4))
print(buffer_start(problem, size=14))
