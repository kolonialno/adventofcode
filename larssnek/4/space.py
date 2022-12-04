with open("input.txt", "r") as f:
    data = f.read().strip()


def compare_sets(type = 'contained'):
    c = 0
    for pair in data.split("\n"):
        s1, s2 = [set(range(int(sec.split("-")[0]), int(sec.split("-")[1])+1)) for sec in pair.split(",")]
        if type == 'contained' and (s1.intersection(s2) == s1 or s1.intersection(s2) == s2):
            c += 1
        elif type == 'overlap' and len(s1.intersection(s2)) > 0:
            c += 1
    return c

# case 1
print(compare_sets(type = 'contained'))

# case 2
print(compare_sets(type = 'overlap'))
