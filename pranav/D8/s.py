def func1():
    with open("input.txt") as f:
        lines = f.readlines()
        
    points = [line.strip().split(' | ')[1] for line in lines]
    final = []
    for p in points:
        final.extend(p.split())
    points = [p for p in final if len(p) in [2,4,3,7]]
    return len(points)



def deduce(input):
    conversion = {}

    # Get 1, 4, 7 and 8
    one = ''.join(d for d in input.split() if len(d) == 2)
    four = ''.join(d for d in input.split() if len(d) == 4)
    seven = ''.join(d for d in input.split() if len(d) == 3)
    eight = ''.join(d for d in input.split() if len(d) == 7)

    # first is A, by looking at 1 and 7
    conversion['a'] = ''.join(set(seven) - set(one)) 
    conversion['c'] = ''.join(one)
    conversion['f'] = ''.join(one)


    # B/D, by looking at 4
    b_or_d = ''.join(set(four) - set(one)) 
    conversion['b'] = b_or_d
    conversion['d'] = b_or_d

    # E/G, by looking at 8
    e_or_g = ''.join(set(eight) - set(four + conversion['a']))
    conversion['e'] = e_or_g
    conversion['g'] = e_or_g

    # Figure out G by finding out the 9.
    digits = [d for d in input.split() if len(d) == 6]
    for d in digits:
        g = ''.join(set(d) - set(four + conversion['a']))
        if len(g) == 1:
            print(g)
            conversion['g'] = g
            break

    # Fix E.
    conversion['e'] = ''.join(set(conversion['e'])-set(conversion['g']))

    # Figure out B by looking at 0.
    digits = [d for d in input.split() if len(d) == 6]
    for d in digits:
        b = ''.join(set(d) - set(one + conversion['a'] + conversion['g'] + conversion['e']))
        if len(b) == 1:
            conversion['b'] = b
            break
        
    # Fix D.
    conversion['d'] = ''.join(set(conversion['d'])-set(conversion['b']))

    # Figure out F by looking at 6.
    digits = [d for d in input.split() if len(d) == 6]
    for d in digits:
        f = ''.join(set(d) - set(conversion['a'] + conversion['b'] + conversion['d']+ conversion['e'] + conversion['g']))
        if len(f) == 1:
            conversion['f'] = f
            break

    # Fix C.
    conversion['c'] = ''.join(set(conversion['c'])-set(conversion['f']))

    return conversion

def decipher(input, output):
    conversion = deduce(input)
    result = []
    zero = sorted(''.join(conversion[l] for l in 'abcefg'))
    six = sorted(''.join(conversion[l] for l in 'abdefg'))
    nine = sorted(''.join(conversion[l] for l in 'abcdfg'))
    two = sorted(''.join(conversion[l] for l in 'acdeg'))
    three = sorted(''.join(conversion[l] for l in 'acdfg'))
    five = sorted(''.join(conversion[l] for l in 'abdfg'))
    for d in output.strip().split():
        d = sorted(d)
        if len(d) == 2: result.append(1)
        if len(d) == 4: result.append(4)
        if len(d) == 3: result.append(7)
        if len(d) == 7: result.append(8)
        if d == zero: result.append(0)
        if d == six: result.append(6)
        if d == nine: result.append(9)
        if d == two: result.append(2)
        if d == three: result.append(3)
        if d == five: result.append(5)

    return result[0]*1000 + result[1]*100 + result[2]*10 + result[3]*1

def func2():
    with open("input.txt") as f:
        lines = f.readlines()

    points = [line.strip().split(' | ') for line in lines]
    results = [decipher(p[0], p[1]) for p in points]
    return sum(results)

    

        
print(func2())