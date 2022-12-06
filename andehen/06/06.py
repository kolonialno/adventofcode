def get_marker(chars, n):

    current = list(chars[0:(n-1)])
    for i in range(n - 1, len(chars)):
        if len(set(current)) == (n-1) and chars[i] not in current:
            return i + 1
        current.pop(0)
        current.append(chars[i])


with open("input.txt") as f:
    data = f.read()

    print(get_marker(data, 4))
    print(get_marker(data, 14))
