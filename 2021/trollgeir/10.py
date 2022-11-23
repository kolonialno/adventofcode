from collections import Counter
from functools import reduce

flip = {"(": ")", "{": "}", "<": ">", "[": "]"}
# Make the dictionary bidirectional
flip.update(dict([reversed(i) for i in flip.items()]))


def autocomplete(data):
    stack = []
    for b in data:
        if b in "[{(<":
            stack.append(b)
        else:
            if stack[-1] != flip[b]:
                raise SyntaxError(b)
            stack.pop()
    return "".join([flip[i] for i in reversed(stack)])


completion_score = {")": 1, "]": 2, "}": 3, ">": 4}
penalty_counter = Counter()
total_score = []

with open("inputs/10.txt") as f:
    for line in f.read().splitlines():
        try:
            segment = autocomplete(line)
            score = reduce(lambda a, b: a * 5 + completion_score[b], segment, 0)
            total_score.append(score)
        except SyntaxError as e:
            penalty_counter.update(str(e))

penalty = {")": 3, "]": 57, "}": 1197, ">": 25137}

answer1 = sum([(penalty[k] * v) for k, v in penalty_counter.items()])
answer2 = sorted(total_score)[int(len(total_score) / 2)]

print(f"Answer1: {answer1}")  # 290691
print(f"Answer2: {answer2}")  # 2768166558
