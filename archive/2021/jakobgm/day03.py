from collections import Counter
from pathlib import Path

# Read and parse input
p = Path("inputs/03.txt")
rows = p.read_text().strip().split("\n")
columns = list("".join(x) for x in zip(*rows))

# Task 1
gamma = "".join(Counter(column).most_common(1)[0][0] for column in columns)
epsilon = "".join("0" if bit == "1" else "1" for bit in gamma)
print(int(gamma, 2) * int(epsilon, 2))

# Task 2
def most_common_bit(string):
    occurences = Counter(string).most_common(2)
    if len(occurences) == 1:
        return occurences[0][0]
    if occurences[0][1] == occurences[1][1]:
        return "1"
    return occurences[0][0]

def rating(columns: list[str], rows: list[str], oxygen: bool) -> int:
    for i in range(0, len(columns)):
        bit_criteria = most_common_bit(columns[i])
        if oxygen:
            rows = [row for row in rows if row[i] == bit_criteria]
        else:
            rows = [row for row in rows if row[i] != bit_criteria]
        columns = list("".join(x) for x in zip(*rows))
        if len(rows) == 1:
            return int(rows[0], 2)
    raise RuntimeError

print(rating(columns, rows, oxygen=True) * rating(columns, rows, oxygen=False))
