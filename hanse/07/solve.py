from sys import stdin

numbers = [int(x) for x in next(stdin).strip().split(",")]


def solve(cost):
    min_cost = float("inf")
    for k in range(max(numbers)):
        min_cost = min(sum(cost(abs(x - k)) for x in numbers), min_cost)
    return min_cost


def a():
    return solve(lambda x: x)


def b():
    return solve(lambda x: x * (x + 1) // 2)


print(a())
print(b())
