
with open("input.txt") as f:
    lines = f.readlines()

def split(r):
    return [int(c) for c in r]
    
m = [split(r.rstrip()) for r in lines]

def peers(mm, i, j):
    poss = [(i-1,j-1),  (i,j-1),    (i+1,j-1),
            (i-1,j),                (i+1,j),
            (i-1,j+1),  (i,j+1),    (i+1,j+1)]
    return [(x,y) for x,y in set(poss) if x >= 0 and y >=0 and x < len(mm[0]) and y < len(mm)]

def reset(m, flashes):
    for x,y in flashes:
        m[x][y] = 0

def increment(mm, i, j, flashes):
    mm[i][j] += 1
    if mm[i][j] == 10:
        flashes.append((i,j))
        for x,y in peers(mm, i, j):
            increment(mm, x, y, flashes)
    return flashes 


def func1():
    score = 0
    for _ in range(100):
        flashes = []
        for i in range(len(m[0])):
            for j in range(len(m)):
                increment(m, i, j, flashes)
        score += len(set(flashes))
        reset(m, flashes)
    return score


def func2():
    for r in range(1000):
        flashes = []
        for i in range(len(m[0])):
            for j in range(len(m)):
                increment(m, i, j, flashes)
        if len(set(flashes)) == 100:
            return r + 1
        reset(m, flashes)
    return 0

# print(func1())
# print(func2())