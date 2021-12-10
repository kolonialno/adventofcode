
left = set(["(", "{" ,"[", "<"])
right = set([")", "}" ,"]", ">"])
left_to_right = { "(": ")", "{" :"}" ,"[" :"]", "<" : ">"}
penalties = {")": 3, "]": 57, "}": 1197, ">": 25137}
scores = {")": 1, "]": 2, "}": 3, ">": 4}

def solve_1(filename):
    return_value = 0
    with open(filename) as file:
        for line in file.readlines():
            stack = []
            for c in line.rstrip():
                if c in left:
                    stack.append(c)
                    continue

                if c in right:
                    t = stack.pop()

                    if left_to_right[t] == c:
                        continue

                    else:
                        return_value += penalties[c]
                        break

                raise ValueError(f"c={c} not in left or right set")


    return return_value

def calc_score(completion_string):
    return_value = 0
    for c in completion_string:
        return_value = 5 * return_value + scores[c]
    return return_value

def solve_2(filename):
    errors = []
    with open(filename) as file:
        for line in file.readlines():
            stack = []
            for c in line.rstrip():
                if c in left:
                    stack.append(c)
                    continue

                if c in right:
                    t = stack.pop()

                    if left_to_right[t] == c:
                        continue

                    else:
                        stack = []
                        break

                raise ValueError(f"c={c} not in left or right set")

            # examine stack
            completion_string = [left_to_right[c] for c in stack[::-1]]
            score = calc_score(completion_string)
            if score > 0:
                errors.append(score)


    return sorted(errors)[len(errors)//2]


assert solve_1("test_input.txt") == 26397
print("solution problem 1:", solve_1("input.txt"))

assert solve_2("test_input.txt") == 288957
print("solution problem 2:", solve_2("input.txt"))
