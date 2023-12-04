from collections import defaultdict
total_worth = 0
cards = defaultdict(lambda: 0)
for i,line in enumerate(out.splitlines()):
    cards[i+1] += 1
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

    print(len(matching_numbers))

    for j in range(len(matching_numbers)):
        print("before", cards[i+1+j])
        cards[i+2+j] += cards[i+1]
        print("before", cards[i+1+j])
    # print(cards, i, flush=True)

print(sum(cards.values()))