#!/bin/python
from collections import defaultdict
with open('./04_input.txt') as f:
    data = f.read().split('\n')


vals = list(map(int, data.pop(0).split(',')))


tables = [[]]
for d in data:
    if d:
        row = list(map(int, d.split()))
        tables[-1].append(row)
    else:
        if tables[-1] != []:
            tables.append([])


if tables[-1] == []:
    tables.pop(-1)


def check_table(table, vals_):
    found_mask = [[0]*5 for row in table]
    for i_row, row in enumerate(table):
        for i_cell, cell in enumerate(row):
            if cell in vals_:
                found_mask[i_row][i_cell] = 1
    # Check row
    for row in found_mask:
        if all(row):
            return True
    # Check column
    for pos in range(5):
        found = []
        for row in table:
            found.append(row[pos] in vals_)
        if all(found):
            return True
    return False


def get_winning_table(get_first=True):
    vals_accumulated = []
    won_tables = []
    solved_by_tries = defaultdict(list)
    for try_, val in enumerate(vals):
        vals_accumulated.append(val)
        for i, table in enumerate(tables):
            if i in won_tables:
                continue
            if check_table(table, vals_accumulated):
                won_tables.append(i)
                solved_by_tries[try_].append(i)
                ret = get_first or (len(won_tables) == len(tables))
                if ret:
                    return table, val, vals_accumulated


def get_unmarked_sum(winning_, vals_accumulated_):
    unmarked = []
    for row in winning_:
        unmarked.extend([cell for cell in row if cell not in vals_accumulated_])
    return sum(unmarked)


winning, val, vals_accumulated = get_winning_table()
winning_last, val_last, vals_accumulated_last = get_winning_table(get_first=False)


unmarked_sum = get_unmarked_sum(winning, vals_accumulated)
unmarked_sum_last = get_unmarked_sum(winning_last, vals_accumulated_last)


print(f'Unmarked sum: {unmarked_sum}, last val: {val}, answer: {unmarked_sum * val}')
print(f'Squid unmarked sum: {unmarked_sum_last}, squid last val: {val_last}, squid answer: {unmarked_sum_last * val_last}')
