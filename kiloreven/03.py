#!/usr/bin/env python
from collections import defaultdict, Counter

with open('./03_input.txt') as f:
    data = f.read().split('\n')

data = [d for d in data if d]

gamma = '0b'
epsilon = '0b'

nums = [0] * len(data[0])
for d in data:
    for i, n in enumerate(d):
        nums[i] += int(n)

for val in nums:
    most_common = val > (len(data) / 2)
    gamma += str(int(most_common))
    epsilon += str(int(not most_common))

gamma_int = int(gamma, 2)
epsilon_int = int(epsilon, 2)

print(f'Gamma: {gamma} ({gamma_int}), epsilon: {epsilon} ({epsilon_int}), answer: {gamma_int * epsilon_int}')


# Part 2
def filter_by_position(lst, position, use_most_common):
    sublist = [item[position] for item in lst]
    counter = Counter(sublist)
    counter_vals = list(counter.values())

    if counter_vals[0] == counter_vals[1]:
        filter_value = '1' if use_most_common else '0'
    else:
        counter_sorted = sorted(counter, key=lambda x: counter[x])
        filter_value = counter_sorted[1 if use_most_common else 0]

    return [item for item in lst if item[position] == filter_value]

def get_rating_value(nums_, most_common=True):
    _nums_filtered = [*nums_]
    for pos in range(len(nums_[0])):
        _nums_filtered = filter_by_position(_nums_filtered, pos, most_common)
        if len(_nums_filtered) == 1:
            return _nums_filtered
    return _nums_filtered

oxygen = f'0b{get_rating_value(data)[0]}'
co2 = f'0b{get_rating_value(data, False)[0]}'

oxygen_int = int(oxygen, 2)
co2_int = int(co2, 2)

print(f'Oxygen: {oxygen} ({oxygen_int}), CO2: {co2} ({co2_int}), answer: {oxygen_int * co2_int}')
