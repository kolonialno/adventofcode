S = [0] * 9
with open("input.txt") as f:
    for f in f.readline().split(","):
        S[int(f)] += 1

s = list(S)
for d in range(1, 257):
    n = s[0]
    s = s[1:] + [n]
    s[6] += n
    if d == 80:
        print(sum(s))

print(sum(s))
