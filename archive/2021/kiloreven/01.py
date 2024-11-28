#!/usr/bin/env python

with open('01_input.txt') as f:
    d = f.read().split('\n')

result = len([x for i, x in enumerate(d) if i == 0 or x > d[i-1]])
print(result)
