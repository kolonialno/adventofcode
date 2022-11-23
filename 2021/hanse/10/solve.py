from sys import stdin

lines = [line.strip() for line in stdin]

PAIRS = {"(": ")", "[": "]", "{": "}", "<": ">"}


def a():
    points = {
        ")": 3,
        "]": 57,
        "}": 1197,
        ">": 25137,
    }

    illegal = []
    for line in lines:
        stack = []
        for c in line:
            if c in PAIRS:
                stack.append(PAIRS[c])
            elif c != stack.pop():
                illegal.append(c)
                break

    return sum(points[c] for c in illegal)


def b():
    points = {
        ")": 1,
        "]": 2,
        "}": 3,
        ">": 4,
    }

    illegal = []
    incomplete = []
    for line in lines:
        stack = []
        corrupted = False
        for c in line:
            if c in PAIRS:
                stack.append(PAIRS[c])
            elif c != stack.pop():
                illegal.append(c)
                corrupted = True

        if not corrupted:
            incomplete.append(list(reversed(stack)))

    scores = []
    for remaining in incomplete:
        score = 0
        for c in remaining:
            score *= 5
            score += points[c]

        scores.append(score)

    return sorted(scores)[len(scores) // 2]


print(a())
print(b())
