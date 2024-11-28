def get_first_digit(input_string):
    for char in input_string:
        if char.isdigit():
            return char
    return None


sum = 0
for string in out:
    a = get_first_digit(string)
    b = get_first_digit(reversed(string))
    sum += int(a+b)
print(sum)
