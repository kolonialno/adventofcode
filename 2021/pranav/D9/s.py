def split(r):
    return [int(c) for c in r]

def peers(m, x, y):
    directions = [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
    pp = []
    for point in directions:
        i, j = point
        if (i>=0 and j>=0 and j<len(m[0]) and i<len(m)):
            pp.append(m[i][j])
    return pp

def bassin(m, x, y):
    if x<0 or y<0 or y>=len(m[0]) or x>=len(m) or m[x][y] == 9:
        return []
    v = m[x][y]
    m[x][y] = 9
    return [v, *bassin(m, x+1,y), *bassin(m, x,y+1), *bassin(m, x-1,y), *bassin(m, x,y-1)]

def func1():
    with open("input.txt") as f:
        lines = f.readlines()

    m = [split(r.strip()) for r in lines]
    result = []
    for i in range(len(m)):
        for j in range(len(m[0])):
            v = m[i][j]
            pp = peers(m, i, j)
            if v not in pp and min(v, *pp) == v:
                result.append(v)
    return sum(result) + len(result)

def func2():
    with open("input.txt") as f:
        lines = f.readlines()

    m = [split(r.strip()) for r in lines]
    result = []
    for i in range(len(m)):
        for j in range(len(m[0])):
            v = m[i][j]
            if v != 9:
                pp = bassin(m, i, j)
                result.append(pp)
    three = [len(x) for x in sorted(result, key=len)[-3:]]
    return three[0]*three[1]*three[2]


print(func2())