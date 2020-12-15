from collections import defaultdict, deque

numbers = [18,8,0,5,4,1,20]
spoken = defaultdict(
    lambda: deque([], 2),
    {i: deque([idx], 2) for idx, i in enumerate(numbers, 1)}
)
last_number = numbers[-1]
c = len(numbers) + 1

# Part 1
# while c <= 2020:

# Part 2 (lol?)
while c <= 30000000:
    idxs = spoken[last_number]
    last_number = 0 if len(idxs) == 1 else idxs[-1] - idxs[-2]
    spoken[last_number].append(c)
    c+= 1

print(last_number)
