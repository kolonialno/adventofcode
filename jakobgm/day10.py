from pathlib import Path

puzzle = [
    list(line)
    for line
    in Path("inputs/10.txt").read_text().strip().split("\n")
]

openers = "([{<"
closers = ")]}>"
correct_opener = {closer: opener for closer, opener in zip(closers, openers)}
syntax_error_scoring = {
    closer: score
    for closer, score
    in zip(closers, (3, 57, 1197, 25137))
}

syntax_error_score = 0
autocompletion_scores = []

for line in puzzle:
    stack = []
    for operator in line:
        if operator in openers:
            stack.append(operator)
            continue
        elif stack.pop() != correct_opener[operator]:
            syntax_error_score += syntax_error_scoring[operator]
            break
    else:
        autocompletion_score = 0
        for opener in reversed(stack):
            autocompletion_score *= 5
            autocompletion_score += openers.find(opener) + 1
        autocompletion_scores.append(autocompletion_score)

print(syntax_error_score)
print(sorted(autocompletion_scores)[len(autocompletion_scores) // 2])
