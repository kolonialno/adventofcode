def build_matrix():
    with open("input.txt") as f:
        lines = f.readlines()
    points = []
    for l in lines:
        l = l.rstrip()
        if not l or l.startswith('fold'):
            continue
        x, y = l.split(',')
        points.append((int(x), int(y)))
    return points

def build_instructions():
    with open("input.txt") as f:
        lines = f.readlines()
    ins = []
    for l in lines:
        l = l.rstrip()
        if not l.startswith('fold'):
            continue
        x, y = l.replace('fold along ', '').split('=')
        ins.append((x, int(y)))
    return ins

def fold_up(m, fold):
    mm = sorted(m, key=lambda y:y[1])
    max_depth = mm[-1][1]
    for p in reversed(mm):
        y = p[1]
        if y > fold:
            mm.append((p[0], max_depth-y))
            mm.remove(p)
    return set(mm)

def fold_left(m, fold):
    mm = sorted(m)
    max_width = mm[-1][0]
    for p in reversed(mm):
        x = p[0]
        if x > fold:
            mm.append((max_width-x, p[1]))
            mm.remove(p)
    return set(mm)

def pprint(m):
    mm = sorted(m, key=lambda y:y[1])
    max_depth = mm[-1][1] + 1
    mm = sorted(m)
    max_width = mm[-1][0] + 1
    
    output = [['.' for _ in range(max_width)] for _ in range(max_depth)]
    for p in mm:
        output[p[1]][p[0]] = '#'
    for o in output:
        print(''.join(str(o)))



def func1():
    pp = build_matrix()
    ii = build_instructions()
    for i in ii:
        if i[0] == 'y':
            pp = fold_up(pp, i[1])
        if i[0] == 'x':
            pp = fold_left(pp, i[1])
    pprint(pp)

func1()