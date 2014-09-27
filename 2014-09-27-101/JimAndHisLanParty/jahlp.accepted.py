import sys

def read():
    l = sys.stdin.readline()
    if l[-1] == '\n': l = l[:-1]
    xs = filter(lambda x: len(x) > 0, l.split(' '))
    return map(int, xs)

n, m, q = read()

ps = map(lambda x: x - 1, read())

gs = [set() for ix in range(m)]

for ix in range(len(ps)):
    gs[ps[ix]].add(ix)

uf = []

for ix in range(len(ps)):
    uf.append([ix, 0, set([ps[ix]])])

res = []

for ix in range(len(gs)):
    if len(gs[ix]) < 2:
        res.append(0)
    else:
        res.append(-1)

def find(x):
    if uf[x][0] == x:
        return x
    r = find(uf[x][0])
    uf[x][0] = r
    return r

def union(u, v, ix):
    ur = find(u)
    vr = find(v)
    ur, uh, us = uf[ur]
    vr, vh, vs = uf[vr]
    if uh > vh:
        uf[vr][0] = ur
        uf[ur][2] |= vs
        for g in vs:
            gs[g].discard(vr)
            gs[g].add(ur)
            if res[g] < 0 and len(gs[g]) == 1:
                res[g] = ix + 1
    elif vh > uh:
        uf[ur][0] = vr
        uf[vr][2] |= us
        for g in vs:
            gs[g].discard(ur)
            gs[g].add(vr)
            if res[g] < 0 and len(gs[g]) == 1:
                res[g] = ix + 1
    else:
        uf[vr][0] = ur
        uf[ur][1] += 1
        uf[ur][2] |= vs
        for g in vs:
            gs[g].discard(vr)
            gs[g].add(ur)
            if res[g] < 0 and len(gs[g]) == 1:
                res[g] = ix + 1

for ix in range(q):
    u, v = map(lambda x: x - 1, read())
    union(u, v, ix)

for r in res:
    print r

