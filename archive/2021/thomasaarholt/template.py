import numpy as np

test = """"""
test1_result = 1
test2_result = 2

with open("inputs/XX.txt") as f:
    txt = f.read().strip().split("\n")

def read(txt):
    txt = np.array(
        txt
        )
    return txt

def func1(txt):
    code = read(txt)
    return 

def test1():
    result = func1(test)
    assert result == test1_result
    print("TEST 1 PASS")

def func2(txt):
    code = read(txt)
    return 

def test2():
    result = func2(test)
    assert result == test2_result
    print("TEST 2 PASS")

test1()
print(func1(txt))
test2()
print(func2(txt))