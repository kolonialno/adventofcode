with open("input.txt") as f:
    lines = f.readlines()
    final = {}
    
    for l in lines: 
        k, v = l.split()
        final[k] = final.get(k, 0) + int(v)

    print(final['forward']*(final['down']-final['up']))


with open("input.txt") as f:
    lines = f.readlines()
    aim = 0
    final = {}
    
    for l in lines: 
        k, v = l.split()
        if k =='forward':
            final[k] = final.get(k, 0) + int(v)

        # Modify aim.
        if k == 'down':
            aim += int(v)
        elif k == 'up':
            aim -= int(v)
        elif k == 'forward' and aim != 0:
            final['down'] = final.get('down', 0) + (aim*int(v)) 

    print(final['forward']*final['down'])
