with open("input.txt") as f:
    data = [[int(x) for x in l.split(" ") if x != " "] for l in f.read().splitlines()]

def derivate(numbers):
    return [numbers[i+1]-numbers[i] for i in range(len(numbers)-1)]


def all_zeros(numbers):
    return all([x == 0 for x in numbers])


def get_next_number(numbers):
    if all_zeros(numbers):
        return 0

    derivative = derivate(numbers)
    return numbers[-1] + get_next_number(derivative)


def get_prev_number(numbers):
    if all_zeros(numbers):
        return 0

    derivative = derivate(numbers)
    return numbers[0] - get_prev_number(derivative)

print(sum([get_next_number(n) for n in data]))
print(sum([get_prev_number(n) for n in data]))
