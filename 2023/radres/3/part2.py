
gear_location_to_numbers = {}
def find_star(i,j, lines, gear_location_to_numbers, or_i, or_j):
    all_i = [1,0,-1]
    all_j = [1,0,-1]
    # pairs = [(1,0), (-1,0), (0,1), (0,-1)]
    for shift_i in all_i:
        for shift_j in all_j:
            shift_j = shift_j
            # for shift_i, shift_j in pairs:
            try:
                if lines[i+shift_i][j+shift_j] == "*":
                    key = (i+shift_i, j+shift_j)
                    if key in gear_location_to_numbers:
                        list_of_pairs = gear_location_to_numbers[key]
                        if (or_i, or_j) not in list_of_pairs:
                            list_of_pairs.append((or_i, or_j))
                    else:
                        gear_location_to_numbers[key] = []
                        list_of_pairs = gear_location_to_numbers[key]
                        if (or_i, or_j) not in list_of_pairs:
                            list_of_pairs.append((or_i, or_j))
            except:
                pass
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
part_locations = {}
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
                part_locations[(i,j-len(number))] = number
                
            number = []
            is_part = False

    if len(number) >0 and is_part:
        sum += int("".join(number))
        part_locations[(i,j-len(number))] = number
                

             
number_coordinates_to_first_coordinate = {}
for key in part_locations:
    i,j = key
    number_coordinates_to_first_coordinate[(i,j)] = [i,j]
    for shift in range(len(part_locations[key])):
        number_coordinates_to_first_coordinate[(i,j+shift)] = [i,j]


for key in number_coordinates_to_first_coordinate:
    i,j= key
    or_i, or_j = number_coordinates_to_first_coordinate[key]
    find_star(i,j,lines,gear_location_to_numbers, or_i, or_j)

sum_mult = 0
for key in gear_location_to_numbers:
    value = gear_location_to_numbers[key]
    if len(value) == 2:
        num1i, num1j = value[0]
        num2i, num2j = value[1]
        sum_mult += int("".join(part_locations[(num1i,num1j)])) * int("".join(part_locations[(num2i,num2j)]))
        

print(sum_mult)
