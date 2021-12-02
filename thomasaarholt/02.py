import numpy as np

test = """forward 5
down 5
forward 8
up 3
down 8
forward 2"""
test1_result = 150
test2_result = 900

try:
    with open("inputs/02.txt") as f:
            txt = f.read()
except:
    print("Download input and specify filename")

def read(txt):
    txt = [
            i.split(" ") for i in txt.split("\n") if i != ""
            ]
    return txt

def func1(txt):
    code = read(txt)

    horizontal = 0
    depth = 0

    for direction, distance in code:
        distance = int(distance)
        if direction == "forward":
            horizontal += distance
        elif direction == "down":
            depth += distance
        elif direction == "up":
            depth -= distance
        else:
            raise ValueError("Something wrong with input")
    return  horizontal * depth

def test1():
    result = func1(test)
    print(result)    
    assert result == test1_result
    print("TEST 1 PASS")

def func2(txt):
    code = read(txt)

    horizontal = 0
    depth = 0
    aim = 0

    for direction, distance in code:
        distance = int(distance)
        if direction == "forward":
            horizontal += distance
            depth += aim*distance

        elif direction == "down":
            aim += distance
        elif direction == "up":
            aim -= distance

        else:
            raise ValueError("Something wrong with input")
    return  horizontal * depth

def test2():
    result = func2(test)
    print(result)
    assert result == test2_result
    print("TEST 2 PASS")

test1()
print(f"RESULT1: {func1(txt)}")
test2()
print(f"RESULT2: {func2(txt)}")
