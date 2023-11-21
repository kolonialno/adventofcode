def is_correct_order(left, right): 

    if isinstance(left, int) and isinstance(right, int):
        if left == right:
            return None 
        return left < right

    if isinstance(left, int) and isinstance(right, list):
        return is_correct_order([left], right)
    if isinstance(left, list) and isinstance(right, int):
        return is_correct_order(left, [right])

    i = 0
    while True:
        if len(left) <= i and len(right) <= i:
            return None
        if len(left) <= i:
            return True
        if len(right) <= i:
            return False

        res = is_correct_order(left[i], right[i])
        if res is not None:
               return res
        i += 1



def problem_1(filename):
    sum = 0
    index = 1
    with open(filename) as file:
        while True:
            left = eval(next(file))
            right = eval(next(file))
            sum += index if is_correct_order(left,right) is True else 0
            index += 1

            try:
                next(file)
            except Exception as exc:
                break
    return sum


import bisect

class Sig(object):
    def __init__(self, value):
        self.s= value
    def __lt__(self, other):
        return is_correct_order(self.s ,other.s)

def problem_2(filename):
    a = Sig([[2]])
    b= Sig([[6]])
    signals = [a,b]
    with open(filename) as file:
        for line in file.readlines():
            if line == "\n":
                continue
            sig = Sig(eval(line))
            bisect.insort(signals, sig)

    return (signals.index(a) + 1) * (signals.index(b) +1)

test_sum = problem_1("test_input.txt")
assert test_sum == 13, f"Test failed with {test_sum}"
print("Solution to problem 1:", problem_1("input.txt"))

test_sum = problem_2("test_input.txt")
assert test_sum == 140, f"Test failed with {test_sum}"
print("Solution to problem 2:", problem_2("input.txt"))
