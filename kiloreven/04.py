#!/bin/python
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
    for row in found_mask:
        if all(row):
            return True
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
    for val in vals:
        vals_accumulated.append(val)
        for i, table in enumerate(tables):
            if i in won_tables:
                continue
            if check_table(table, vals_accumulated):
                won_tables.append(i)
                ret = get_first or (len(won_tables) == len(tables))
                if ret:
                    return table, val, vals_accumulated


def get_unmarked_sum(winning, val, vals_accumumated):
    unmarked = []
    for row in winning:
        unmarked.extend([cell for cell in row if cell not in vals_accumulated])
    return sum(unmarked)


winning, val, vals_accumulated = get_winning_table()
winning_last, val_last, vals_accumulated_last = get_winning_table(get_first=False)

unmarked_sum = get_unmarked_sum(winning, val, vals_accumulated)
unmarked_sum_last = get_unmarked_sum(winning_last, val_last, vals_accumulated_last)

# print(vals_accumulated_last)
# for row in winning_last:
#     print([f'{int(cell in vals_accumulated_last)}: {cell}' for cell in row])


print(f'Unmarked sum: {unmarked_sum}, last val: {val}, answer: {unmarked_sum * val}')
print(f'Squid unmarked sum: {unmarked_sum_last}, squid last val: {val_last}, squid answer: {unmarked_sum_last * val_last}')
