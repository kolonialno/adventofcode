def get_marker(chars, n):
    for i in range(len(chars) - n):
        if len(set(chars[i : (i + n)])) == n:
            return i + n


with open("input.txt") as f:
    data = f.read()

print(get_marker(data, 4))
print(get_marker(data, 14))
