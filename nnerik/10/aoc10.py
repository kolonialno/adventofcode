def chunk(input):
    first, rest = input[0], input[1:]
    score1, score2 = 0, 0
    while rest and rest[0] in "([{<":
        rest, score1, score2 = chunk(rest)
        if score1:
            break
    if score1:
        return "", score1, 0
    if not rest:
        return "", 0, 5 * score2 + {"(": 1, "[": 2, "{": 3, "<": 4}[first]
    if rest[0] == {"{": "}", "[": "]", "(": ")", "<": ">"}[first]:
        return rest[1:], 0, score2
    return "", {")": 3, "]": 57, "}": 1197, ">": 25137}[rest[0]], 0


scores = [chunk(line.strip())[1:] for line in open("input.txt")]
print("Part 1:", sum(r for r, _ in scores))

score2s = sorted(r for _, r in scores if r)
print("Part 2:", score2s[len(score2s) // 2])
