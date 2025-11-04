input = open(0).read().strip().split("\n")
equations = []
for line in input:
    left, right = line.split(":")
    equations.append((int(left), [int(n) for n in reversed(right.split())]))


def round_up_to_power_of_10(n):
    if n < 10:
        return 10
    return 10 * round_up_to_power_of_10(n // 10)


def search(value, numbers, with_concat=False):
    if not numbers or value <= 0:
        return 0

    # Search for solution using + and *
    if (
        value == numbers[0]
        or search(value - numbers[0], numbers[1:], with_concat)
        or value % numbers[0] == 0
        and search(value // numbers[0], numbers[1:], with_concat)
    ):
        return value

    # Search for solution using concatenation
    if with_concat:
        modulo = round_up_to_power_of_10(numbers[0])
        if value % modulo == numbers[0] and search(
            value // modulo, numbers[1:], with_concat
        ):
            return value

    return 0


print("Part 1:", sum(search(*equation) for equation in equations))
print("Part 2:", sum(search(*equation, with_concat=True) for equation in equations))
