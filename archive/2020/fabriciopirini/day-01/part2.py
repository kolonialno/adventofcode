from itertools import combinations

f = open('input.txt', 'r')

target = 2020

if f.mode == 'r':
    contents = f.read()
    arr = sorted([int(ele) for ele in contents.split('\n') if int(ele) <= target])

    trioArr = [combination for combination in combinations(arr, 3) if sum(combination) == target]
    a, b, c = trioArr[0]
    result = a * b * c

    print(print({'a': a, 'b': b, 'c': c, 'mult': result}))