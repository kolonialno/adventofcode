import math
from itertools import combinations

numbers = [int(n) for n in open("numbers.txt").read().strip().split("\n")]
print([math.prod(c) for c in combinations(numbers, 2) if sum(c) == 2020][0]) # Part 1
print([math.prod(c) for c in combinations(numbers, 3) if sum(c) == 2020][0]) # Part 2
