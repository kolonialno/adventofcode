def func1():
    with open("input.txt") as f:
        lines = f.readlines()
    transpose = list(zip(*lines))
    middle = len(lines) // 2
    occ = ['1' if sum(map(int, x)) > middle else '0' for x in transpose]
    occ2 = ['0' if sum(map(int, x)) > middle else '1' for x in transpose]
    return int(''.join(occ), 2) * int(''.join(occ2), 2)
        

def func2():
    with open("input.txt") as f:
        lines = f.readlines()
    
    def loopit(high='1', low='0'):
        # fugly
        lines2 = lines.copy()
        i = 0
        while len(lines2) != 1:
            transpose = list(zip(*lines2))
            middle = len(lines2) // 2
            s = sum(map(int, transpose[i]))
            h = high if s > middle or (s == middle and len(lines2) % 2 == 0) else low
            lines2 = [x.strip() for x in lines2 if x[i].startswith(h)]
            i += 1 
        return lines2[0]

    occ = loopit()
    occ2 = loopit(high='0', low='1')
    return int(''.join(occ), 2) * int(''.join(occ2), 2)

