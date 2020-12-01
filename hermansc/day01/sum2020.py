from itertools import combinations

numbers = []
with open("numbers.txt", "r") as fh:
    for n in fh:
        numbers.append(int(n))

combos = combinations(numbers, 3)
for n1, n2, n3 in combos:
    if n1 + n2 + n3 == 2020:
        print(f"{n1} + {n2} + {n3} = {n1 * n2 * n3}")
        break
