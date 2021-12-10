with open("input.txt") as f:
    lines = f.readlines()

def score(c):
    if c == ')':
        return 3
    if c == ']':
        return 57
    if c == '}':
        return 1197
    if c == '>':
        return 25137

def score2(c):
    if c == ')':
        return 1
    if c == ']':
        return 2
    if c == '}':
        return 3
    if c == '>':
        return 4

pairs = {']': '[', '}': '{', ')': '(', '>': '<'}
pairs2 = {'[': ']', '{': '}', '(': ')', '<': '>'}

def func1():
    result = []
    for line in lines:
        stack = []
        for b in line.strip():
            if b in ['{','[','(','<']:
                stack.append(b)
            else:
                r = pairs.get(b)
                if stack[-1] == r:
                    stack.pop()
                else:
                    result.append(score(b))
                    break
                            
    return sum(result)

def func2():
    result = []
    for line in reversed(lines):
        stack = []
        for b in line.strip():
            if b in ['{','[','(','<']:
                stack.append(b)
            else:
                r = pairs.get(b)
                if stack[-1] == r:
                    stack.pop()
                else:
                    lines.remove(line)
                    break

    for line in lines:
        stack = []
        for b in line.strip():
            stack.append(b) if b in ['{','[','(','<'] else stack.pop()
        # Score line.
        rs = [pairs2.get(l) for l in reversed(stack)]
        points = 0
        for r in rs:
            points *= 5
            points += score2(r)
        result.append(points)

    m = int(len(result)/2)
    return sorted(result)[m]


print(func2())