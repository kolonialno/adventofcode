from pathlib import Path
import numpy as np


data = Path("day01.txt").read_text().splitlines()

# Part 1

# data = """1abc2
# pqr3stu8vwx
# a1b2c3d4e5f
# treb7uchet""".split("\n")
numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]


def line_to_num(line):
    only_num = list(filter(lambda x: x in numbers, list(line)))
    return int(f"{only_num[0]}{only_num[-1]}")


numbers = [str(x) for x in numbers]
results = []
for line in data:
    results.append(line_to_num(line))
print(sum(results))

# Part 2
strnumbers = {
    "one": "1",
    "two": "2",
    "three": "3",
    "four": "4",
    "five": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "nine": "9",
    "1": "1",
    "2": "2",
    "3": "3",
    "4": "4",
    "5": "5",
    "6": "6",
    "7": "7",
    "8": "8",
    "9": "9",
}
joined = []
for line in data:
    left_words = []
    left_indices = []
    right_words = []
    right_indices = []
    for word, num in strnumbers.items():
        index = line.find(word)
        if index != -1:
            left_words.append(num)
            left_indices.append(index)
        index = line.rfind(word)
        if index != -1:
            right_words.append(num)
            right_indices.append(index)

    left = left_words[left_indices.index(min(left_indices))]
    right = right_words[right_indices.index(max(right_indices))]
    joined.append(int(left + right))
print(sum(joined))
