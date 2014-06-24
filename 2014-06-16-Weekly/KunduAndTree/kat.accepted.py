import sys

md = 1000000007

n = int(sys.stdin.readline())
m = n - 1

es = {}

for ix in range(m):
    a, b, c = sys.stdin.readline().split(' ')
    a = int(a)
    b = int(b)
    c = c[0]
    if a not in es:
        es[a] = []
    es[a].append((b, c == 'b'))
    if b not in es:
        es[b] = []
    es[b].append((a, c == 'b'))

cs = {}

q = [(1, None)]

vis = set()

while len(q) > 0:
    v, p = q.pop()
    if v in vis:
        continue
    vis.add(v)
    if p is None:
        cs[v] = 1
        p = v
    else:
        cs[p] += 1
    for u, b in es[v]:
        if u in vis:
            continue
        q.append((u, p if b else None))

def n_ch_3(x):
    x1 = x - 1
    x2 = x - 2
    if x % 2 == 0:
        x /= 2
    elif x1 % 2 == 0:
        x1 /= 2
    elif x2 % 2 == 0:
        x2 /= 2
    else:
        assert False
    if x % 3 == 0:
        x /= 3
    elif x1 % 3 == 0:
        x1 /= 3
    elif x2 % 3 == 0:
        x2 /= 3
    else:
        assert False
    return (((x * x1) % md) * x2) % md

def n_ch_2(x):
    x1 = x - 1
    if x % 2 == 0:
        x /= 2
    elif x1 % 2 == 0:
        x1 /= 2
    else:
        assert False
    return (x * x1) % md

res = n_ch_3(n)

for k in cs:
    if cs[k] == 1:
        continue
    res = (res - n_ch_3(cs[k])) % md
    res = (res - ((n_ch_2(cs[k]) * (n - cs[k])) % md)) % md

print res

