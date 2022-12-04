import string
with open("input.txt", "r") as f:
    data = f.read().strip()

# case 1
in_both = [string.ascii_letters.index(l) + 1 for r in data.split("\n") for l in set(r[:len(r)//2]) if l in r[len(r)//2:]]
print(sum(in_both))

# case 2
in_all_three = [string.ascii_letters.index(l) + 1 for i in range(0, len(data.split("\n")), 3) for l in set(data.split("\n")[i]) if l in data.split("\n")[i+1] and l in data.split("\n")[i+2]]
print(sum(in_all_three))
