with open("input.txt") as f:
    data = f.read().split("\n")[:-1]


fully_overlaps = 0
partly_overlaps = 0

for pairs in data:
    e1, e2 = pairs.split(",")
    e1_lo, e1_hi = (int(s) for s in e1.split("-"))
    e2_lo, e2_hi = (int(s) for s in e2.split("-"))
    e1_r = set(range(e1_lo, e1_hi + 1))
    e2_r = set(range(e2_lo, e2_hi + 1))
    c = len(e1_r.intersection(e2_r))
    if c in [len(e1_r), len(e2_r)]:
        fully_overlaps += 1
    if c > 0:
        partly_overlaps += 1

print(fully_overlaps)
print(partly_overlaps)
