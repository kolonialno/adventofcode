d="abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg"
f=lambda p,t:sum({c:p.count(c)for c in"abcdefg"}[s]for s in t)
r=["".join({f(d,t):str(n)for n,t in enumerate(d.split())}[f(p,t)]for
t in o.split())for l in open(0)for p,o in[l.split("|")]]
print(sum(d in"1478"for o in r for d in o),sum(int(n)for n in r))