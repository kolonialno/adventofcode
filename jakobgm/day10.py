from pathlib import Path

puzzle = [
    list(line)
    for line
    in Path("inputs/10.txt").read_text().strip().split("\n")
]

openers = "[({<"
closers = "])}>"
correct_opener = {closer: opener for closer, opener in zip(closers, openers)}
correct_closer = {opener: closer for closer, opener in correct_opener.items()}
syntax_error_scoring = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137,
}
autocompletion_scoring = {
    ")": 1,
    "]": 2,
    "}": 3,
    ">": 4,
}

syntax_error_score = 0
autocompletion_scores = []

uncorrupted_lines = []
for line in puzzle:
    stack = []
    for operator in line:
        if operator in openers:
            stack.append(operator)
        elif operator in closers:
            opener = stack.pop()
            if not correct_opener[operator] == opener:
                syntax_error_score += syntax_error_scoring[operator]
                break
    else:
        autocompletion_score = 0
        for operator in reversed(stack):
            autocompletion_score *= 5
            autocompletion_score += autocompletion_scoring[correct_closer[operator]]
        autocompletion_scores.append(autocompletion_score)

print(syntax_error_score)
print(sorted(autocompletion_scores)[len(autocompletion_scores) // 2])
