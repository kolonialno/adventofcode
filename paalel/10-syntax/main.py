from functools import reduce

left_to_right = { "(": ")", "{" :"}" ,"[" :"]", "<" : ">"}
penalties = {")": 3, "]": 57, "}": 1197, ">": 25137}
scores = {")": 1, "]": 2, "}": 3, ">": 4}

def solve(filename):
    corrupted = 0
    incomplete = []
    with open(filename) as file:
        for line in file.readlines():
            stack = []
            for c in line.rstrip():
                if c in ["(", "{" ,"[", "<"]:
                    stack.append(c)
                    continue

                if left_to_right[stack.pop()] == c:
                    continue

                else:
                    corrupted += penalties[c]
                    stack = []
                    break

            if stack:
                incomplete.append(reduce(lambda s,r: 5*s + scores[r], [left_to_right[c] for c in stack[::-1]], 0))

    return corrupted, sorted(incomplete)[len(incomplete)//2]

assert solve("test_input.txt")[0] == 26397
print("solution problem 1:", solve("input.txt")[0])

assert solve("test_input.txt")[1] == 288957
print("solution problem 2:", solve("input.txt")[1])
