import string


with open("input.txt") as f:
    data = f.read()

rucksacks = data.split("\n")[:-1]

p1 = 0
for r in rucksacks:
    l = len(r) // 2
    i = set(r[:l]).intersection(r[l:])
    p1 += string.ascii_letters.index(i.pop()) + 1

print(p1)

# part 2
p2 = 0
for i in range(len(rucksacks) // 3):
    j = i * 3
    g = rucksacks[j:(j+3)]
    c = set(g[0]).intersection(g[1]).intersection(g[2])
    p2 += string.ascii_letters.index(c.pop()) + 1

print(p2)
