groups = [l.split("\n") for l in open("forms.txt").read().split("\n\n")]

# Part 1
print(sum([len(set([c for line in g for c in line])) for g in groups]))

# Part 2
seen = lambda args: set.intersection(*map(set, args))
print(sum([len(seen([line for line in g])) for g in groups]))
