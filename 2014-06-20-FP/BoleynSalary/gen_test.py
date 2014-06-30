import random
import sys

sys.setrecursionlimit(1000000)

n = 300000
q = 300000

print n, q

sals = range(1, n + 1)
random.shuffle(sals)

cs = {}

for ix in range(n - 1):
    p = random.randrange(1, ix + 2)
    if p not in cs:
        cs[p] = []
    cs[p].append(ix + 2)
    print ix + 2, p

sz = [0 for ix in range(n)]

def f(x):
    if x in cs:
        for v in cs[x]:
            f(v)
        sz[x - 1] = len(cs[x]) + sum(map(lambda x: sz[x - 1], cs[x]))
    else:
        sz[x - 1] = 0

f(1)

print ' '.join(map(str, sals))

d = 0

def col0(x, acc):
    acc.append((sals[x - 1], x))
    if x in cs:
        for v in cs[x]:
            col0(v, acc)

def col(x):
    res = []
    if x in cs:
        for v in cs[x]:
            col0(v, res)
    return res
    
def fnd(v, k):
    xs = col(v)
    xs.sort()
    return xs[k - 1][1]

for ix in range(q):
    v = random.randrange(1, n + 1)
    while sz[v - 1] == 0:
        v = random.randrange(1, n + 1)
    k = random.randrange(1, sz[v - 1] + 1)
    print v - d, k
    d = fnd(v, k)

