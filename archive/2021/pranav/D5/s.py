from typing import Counter


def spread(p1, p2, diagonal=False):
    x1, y1 = [int(i) for i in p1.split(',')]
    x2, y2 = [int(i) for i in p2.split(',')]
    if x1 == x2 and y1 < y2:
        return [(x1, y) for y in range(y1, y2 + 1)]
    if x1 == x2 and y1 > y2:
        return [(x1, y) for y in range(y1, y2 - 1 , -1)]
    if y1 == y2 and x1 < x2:
        return [(x, y1) for x in range(x1, x2 + 1)]
    if y1 == y2 and x1 > x2:
        return [(x, y1) for x in range(x1, x2 - 1, -1)]
    if diagonal:
        if x1 < x2 and y1 < y2:
            points = [(x1,y1), (x2,y2)]
            x,y = x1, y1
            x += 1
            y += 1
            while x != x2 and y != y2:
                points.append((x,y))
                x += 1
                y += 1
            return points

        if x1 < x2 and y1 > y2:
            points = [(x1,y1), (x2,y2)]
            x,y = x1, y1
            x += 1
            y -= 1
            while x != x2 and y != y2:
                points.append((x,y))
                x += 1
                y -= 1
            return points
        
        if x1 > x2 and y1 < y2:
            points = [(x1,y1), (x2,y2)]
            x,y = x1, y1
            x -= 1
            y += 1
            while x != x2 and y != y2:
                points.append((x,y))
                x -= 1
                y += 1
            return points
        
        if x1 > x2 and y1 > y2:
            points = [(x1,y1), (x2,y2)]
            x,y = x1, y1
            x -= 1
            y -= 1
            while x != x2 and y != y2:
                points.append((x,y))
                x -= 1
                y -= 1
            return points

    return []

def high(points):
    c = Counter(points)
    counts = [c for _, c in c.items() if c != 1]
    return len(counts)

def func1():
    with open("input.txt") as f:
        lines = f.readlines()
    points = [spread(*line.strip().split(' -> ')) for line in lines]
    points = [p for l in points for p in l]
    return high(points)

def func2():
    with open("input.txt") as f:
        lines = f.readlines()
    points = [spread(*line.strip().split(' -> '), diagonal=True) for line in lines]
    points = [p for l in points for p in l]
    return high(points)

    

        
# print(func1())
print(func2())