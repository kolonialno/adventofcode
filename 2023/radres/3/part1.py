def check_is_part(i,j, lines):
    all_i = [1,0,-1]
    all_j = [1,0,-1]
    is_part = False
    for shift_i in all_i:
        for shift_j in all_j:
            try:
                if not lines[i+shift_i][j+shift_j].isdigit() and lines[i+shift_i][j+shift_j] != ".":
                    is_part = True
            except:
                pass
    return is_part
    
sum = 0
lines = out.splitlines()
for i,line in enumerate(lines):
    number = []
    is_part = False
    for j,c in enumerate(line):
        if c.isdigit():
            number.append(c)
            if not is_part:
                is_part = check_is_part(i,j, lines)
        else:
            if len(number) > 0 and is_part:
                sum += int("".join(number))
            number = []
            is_part = False

    if len(number) >0 and is_part:
        sum += int("".join(number))
                
print(sum)