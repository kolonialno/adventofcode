#!/usr/bin/env python
from collections import defaultdict


with open('02_input.txt') as f:
    data = f.read().split('\n')

summed = defaultdict(int)
for d in data:
    if not d:
        continue
    key, val = d.split()
    summed[key] += int(val)

vertical = summed['down'] - summed['up']
forward = summed['forward']

print(f'Vertical: {vertical}, forward: {forward}, answer: {vertical * forward}')
