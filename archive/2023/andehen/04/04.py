with open("input.txt") as f:
    data = f.read()

total_points = 0
cards = {k: 1 for k in range(1, len(data.splitlines()) + 1)}
i = 1
for line in data.splitlines():
    _, numbers = line.split(":")
    winning_numbers_str, your_numbers_str = numbers.split("|")
    winning_numbers = [int(x) for x in winning_numbers_str.split(" ") if x.isdigit()]
    your_numbers = [int(x) for x in your_numbers_str.split(" ") if x.isdigit()]

    num_wins = 0
    for n in your_numbers:
        if n in winning_numbers:
            num_wins += 1
            cards[i + num_wins] += cards[i]
    if num_wins > 0:
        total_points += 2 ** (num_wins - 1)

    i += 1

print(total_points)
print(sum([x[1] for x in cards.items()]))
