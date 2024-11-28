import numpy as np

test = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010""".strip().split("\n")
test1_result = 198
test2_result = 230

with open("inputs/03.txt") as f:
    txt = f.read().strip().split("\n")

def read(txt):
    lines = []
    for line in txt:
        newline = [int(l) for l in line]
        lines.append(newline)
    txt = np.array(
        lines
        )
    return txt

def binary_to_int(binary): # awkward
    return int("".join([str(i) for i in binary]), 2)

def func1(txt):
    code = read(txt)
    binary = (code.mean(0) > 0.5)
    return binary_to_int(binary.astype(int)) * binary_to_int((~binary).astype(int))

def test1():
    result = func1(test)
    print(result)
    assert result == test1_result
    print("TEST 1 PASS")

def filter_code(code, is_oxygen):
    for col in range(len(code)):
        mean = code[col].mean()
        if mean >= 0.5:
            mask = code[col] == int(is_oxygen)
        else:
            mask = code[col] == int(not is_oxygen)
        code = code[:, mask]
        if code.shape[1] == 1:
            break
    return code[:, 0]

def func2(txt):
    code = read(txt).T
    ox = binary_to_int(filter_code(code, True))
    co2 = binary_to_int(filter_code(code, False))
    return ox * co2

def test2():
    result = func2(test)
    assert result == test2_result
    print("TEST 2 PASS")

test1()
print(func1(txt))
test2()
print(func2(txt))