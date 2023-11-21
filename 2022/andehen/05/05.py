def parse_move(move):
    m = move.replace("move ", "").replace(" from ", ",").replace(" to ", ",").split(",")
    n = int(m[0])
    f = int(m[1])
    t = int(m[2])
    return n, f, t


def setup_stacks(stacks):
    ns = len(stacks[0])
    S = [[] for s in range(0, ns)]

    for l in reversed(stacks):
        for li, c in enumerate(l):
            if c != " ":
                S[li].append(c)

    return S


# preprocessed in vim
with open("stacks_2.txt") as f:
    stacks = f.read().split("\n")[:-1]

with open("moves.txt") as f:
    moves = [parse_move(m) for m in f.read().strip().split("\n")]



S1 = setup_stacks(stacks)
for n, f, t in moves:
    for a in range(n):
        S1[t - 1].append(S1[f - 1].pop())

print("".join([s.pop() for s in S1]))

S2 = setup_stacks(stacks)
for n, f, t in moves:
    for a in range(-n, 0):
        S2[t - 1].append(S2[f - 1].pop(a))

print("".join([s.pop() for s in S2]))
