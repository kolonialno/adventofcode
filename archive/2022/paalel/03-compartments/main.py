p = lambda x: ord(x) - (38 if ord("A") <= ord(x) <= ord("Z") else 96)
with open("input.txt") as f:
    print(sum(p((set(l[len(l)//2:]) & set(l[:len(l)//2])).pop()) for l in f.readlines()))
with open("input.txt") as f:
    a = [l.rstrip() for l in f.readlines()]
    print(sum(p((set(a[i]) & set(a[i+1]) & set(a[i+2])).pop()) for i in range(0, len(a), 3)))
