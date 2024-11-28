expressions = open("expressions.txt").read().strip().split("\n")

def paren(e):
    start, end = None, None
    start_c, end_c = 0, 0
    for idx, c in enumerate(e):
        if c == "(":
            start_c += 1
            if start is None:
                start = idx
        elif c == ")":
            end_c += 1
            if start is not None and end_c == start_c:
                end = idx + 1
                break

    if start is not None and end is not None:
        return (start, end)
    return (None, None)

def first(nmbrs, op):
    a, b = nmbrs
    if op == "+":
        return (a + b)
    elif op == "*":
        return (a * b)
    return 0

def evaluate(exp, p):
    # Expand all parenthesis
    while True:
        s, e = paren(exp)
        if s is None:
            break
        res = evaluate(exp[s+1:e-1], p=p)
        exp = "".join(exp[:s] + str(res) + exp[e:])

    # Calculate the values
    result = 0
    if p == 1:
        while "+" in exp or "*" in exp:
            parts = exp.split(" ")
            result = first((int(parts[0]), int(parts[2])), parts[1])
            exp = (str(result) + " " + " ".join(parts[3:])).strip()
    elif p == 2:
        while "+" in exp:
            parts = exp.split(" ")
            for n in range(1, len(parts), 2):
                if parts[n] == "+":
                    result = first((int(parts[n-1]), int(parts[n+1])), parts[n])
                    exp = (" ".join(parts[:n-1]) + " " + str(result) + " " + " ".join(parts[n+2:])).strip()

        while "*" in exp:
            parts = exp.split(" ")
            for n in range(1, len(parts), 2):
                if parts[n] == "*":
                    result = first((int(parts[n-1]), int(parts[n+1])), parts[n])
                    exp = (" ".join(parts[:n-1]) + " " + str(result) + " " + " ".join(parts[n+2:])).strip()

    return result

print(sum(evaluate(exp, p=1) for exp in expressions))
print(sum(evaluate(exp, p=2) for exp in expressions))
