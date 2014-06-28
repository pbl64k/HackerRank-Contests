import sys

sys.setrecursionlimit(1000000)

M = 1000000007

def readints(): return map(int, sys.stdin.readline().split(' '))

n = int(sys.stdin.readline())

a = readints()

def conv(x):
    res = []
    while x != 0:
        b = x % 2
        x = x / 2
        if b == 1:
            res.append((1, 0))
        else:
            res.append((0, 1))
    return res

def comb(a, b):
    a1, a0 = a
    b1, b0 = b
    return a1 + b1, a0 + b0

def comball(a, b):
    if len(a) == 0:
        return b
    if len(b) == 0:
        return a
    x = max(len(a), len(b))
    res = []
    for ix in range(x):
        a0 = a[ix] if ix < len(a) else (0, a[0][0] + a[0][1])
        b0 = b[ix] if ix < len(b) else (0, b[0][0] + b[0][1])
        res.append(comb(a0, b0))
    return res

def bt(xs, i, j):
    if i == j:
        return (i, j, xs[i], None, None)
    m = (i + j) / 2
    l = bt(xs, i, m)
    r = bt(xs, m + 1, j)
    return (i, j, comball(l[2], r[2]), l, r)

ca = map(conv, a)

tr = bt(ca, 0, n - 1)

m = int(sys.stdin.readline())

def ft(t, p, r):
    if t is None:
        return []
    i, j, a, lt, rt = t
    if r < i or j < p:
        return []
    if p <= i and j <= r:
        return a
    return comball(ft(lt, p, r), ft(rt, p, r))

def qt(t, k, p, r):
    if p == r:
        return 0
    cs = ft(t, p, r)
    def f(x):
        x1, x0 = x
        return x1 * x0, (x1 * (x1 - 1) / 2) + (x0 * (x0 - 1) / 2)
    zs = map(f, cs)
    res = 0
    for ix in range(max(len(zs), len(k))):
        if ix >= len(zs):
            if k[ix][0] == 1:
                z1 = zs[0][0] + zs[0][1]
            else:
                z1 = 0
        elif ix >= len(k) or k[ix][0] == 0:
            z1, z0 = zs[ix]
        else:
            z0, z1 = zs[ix]
        if z1 > 0:
            res = (res + ((((2 ** ix) % M) * z1) % M)) % M
    return res

for qix in range(m):
    k, p, r = readints()
    print qt(tr, conv(k), p - 1, r - 1)

