from math import ceil
import functools


def chinese_remainder(n, a):
    """
    Chinese remainder implementation stolen from the internet
    """
    sum = 0
    prod = functools.reduce(lambda a, b: a * b, n)
    for n_i, a_i in zip(n, a):
        p = prod // n_i
        sum += a_i * mul_inv(p, n_i) * p
    return sum % prod


def mul_inv(a, b):
    b0 = b
    k0, k1 = 0, 1
    if b == 1:
        return 1
    while a > 1:
        q = a // b
        a, b = b, a % b
        k0, k1 = k1 - q * k0, k0
    if k1 < 0:
        k1 += b0
    return k1


def part1():
    """
    Find t for min(t), t * n >= K for n in N and t in T, where K is some
    natural constant and T is a set for natural numbers.
    """

    K = 1000509
    T = [17, 37, 739, 29, 13, 23, 971, 41, 19]

    gen = ((t * ceil(float(K) / t), t) for t in T)
    nt, t = functools.reduce(lambda a, b: a if a[0] < b[0] else b, gen)
    print("Solution part 1: (", nt, "-", K, ")", "*", t, "=", (nt - K) * t)


def part2():
    """
    Find the smallest k in N s.t
    k + i = n * t_i for some n in N and all t in T=[...]

    Oberservation: all the numbers in T are prime. This implies that the
    solution most satisfy:
    k + i % t_i = 0 for all t in T

    Rewriting this to
    k % t_i = -i for all t in T
    the unique, and therefore optimal, solution can be found using the chinese
    remainder theorem.
    """

    I, T = [], []
    with open("input.txt", "r+") as file:
        for i, t in enumerate(file.readline().rstrip().split(",")):
            if t != "x":
                I.append(-i)
                T.append(int(t))

    print("Solution part 2:", chinese_remainder(T, I))


if __name__ == "__main__":
    part1()
    part2()
