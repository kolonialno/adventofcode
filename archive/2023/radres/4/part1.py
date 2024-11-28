total_worth = 0
for line in out.splitlines():
    line_worth = 0
    if len(line) == 0:
        continue
    winnings = []
    mine = []
    for num in line.split(":")[1].split("|")[0].strip().split(" "):
        if num.isdigit():
            winnings.append(num)
    for num in line.split(":")[1].split("|")[1].strip().split(" "):
        if num.isdigit():
            mine.append(num)

    matching_numbers = []
    for mynum in mine:
        if mynum in winnings:
            matching_numbers.append(mynum)

    if len(matching_numbers) > 0:
        total_worth += pow(2, len(matching_numbers)-1)

print(total_worth)