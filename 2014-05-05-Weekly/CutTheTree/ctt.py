import sys

sys.setrecursionlimit(1000000)

n = int(sys.stdin.readline())
ns = map(int, filter(lambda x: len(x) > 0, sys.stdin.readline().split(' ')))

es = {}

for ix in range(n - 1):
    x = map(int, filter(lambda x: len(x) > 0, sys.stdin.readline().split(' ')))
    a = x[0]
    b = x[1]
    a -= 1
    b -= 1
    if a not in es:
        es[a] = []
    es[a].append(b)
    if b not in es:
        es[b] = []
    es[b].append(a)

m = {}

def calc(a, b):
    if (a, b) not in m:
        s = ns[b]
        for c in es[b]:
            if a == c:
                continue
            s += calc(b, c)
        m[(a, b)] = s
    return m[(a, b)]
    
for a in es:
    for b in es[a]:
        calc(a, b)

best = None

for a, b in m:
    d = abs(m[(a, b)] - m[(b, a)])
    if best is None or d < best:
        best = d

print best

