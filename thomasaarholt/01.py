import numpy as np

test = """199
    200
    208
    210
    200
    207
    240
    269
    260
    263"""

with open("inputs/01.txt") as f:
    code = f.read()

def func1(code):
    code = np.array([int(i) for i in code.split("\n") if i != ""])
    n = (np.diff(code) > 0).sum()
    return n

def func2(code):
    code = [int(i) for i in code.split("\n") if i]
    windows = []
    for i, current in enumerate(code):
        if i < len(code) - 2:
            window = [code[i], code[i + 1], code[i + 2]]
        windows.append(sum(window))
    windows = np.array(windows)
    n = (np.diff(windows) > 0).sum()
    return n

def test1(test):
    print(func1(test))
    assert func1(test) == 7

def test2(test):
    print(func2(test))
    assert func2(test) == 5

assert func1(test) == 7
assert func2(test) == 5

print(func1(code))
print(func2(code))