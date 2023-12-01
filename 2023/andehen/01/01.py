with open("input.txt") as f:
    data = f.read()

# Part 1
s1 = 0
for line in data.splitlines():
    digits = [x for x in line if x.isdigit()]
    d1 = digits[0]
    d2 = digits[-1]
    s1 += int(digits[0] + digits[-1])

print(s1)

# Part 2
numbers = {
    "1": 1, "2": 2, "3": 3, "4": 4, "5": 5,
    "6": 6, "7": 7, "8": 8, "9": 9,
    "one": 1, "two": 2, "three": 3, "four": 4, "five": 5,
    "six": 6, "seven": 7, "eight": 8, "nine": 9
}

s2 = 0
for line in data.splitlines():
    nf = []
    for k, v in numbers.items():
        start = 0
        while True:
            i = line[start:].find(k)
            if i != -1:
                nf.append((i + start, v))
                start = i + start + 1
            else:
                break

    nf.sort(key=lambda x: x[0])
    digits = [x[1] for x in nf]
    s2 += int(str(digits[0]) + str(digits[-1]))

print(s2)
