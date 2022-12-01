with open("input.txt") as f:
    data = f.read()

totals = [sum([int(c) for c in ec.split("\n") if c != ""]) for ec in data.split("\n\n")]
print(max(totals))
print(sum(sorted(totals, reverse=True)[:3]))
