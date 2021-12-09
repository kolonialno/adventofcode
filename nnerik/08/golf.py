d="abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg"
f=lambda p,t:sum(sorted({c:p.count(c)for c in"abcdefg"}[s]for s in t))
k={f(d,t):str(n)for n,t in enumerate(d.split())}
c=lambda p,o:"".join(k[f(p,t)]for t in o.split())
r=[c(*l.split("|"))for l in open("i")]
print(sum(1 for o in r for d in o if d in"1478"),sum(int(n)for n in r))
