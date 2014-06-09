import sys

sys.setrecursionlimit(1000000)

n, m = map(int, sys.stdin.readline().split(' '))

def between(a, b, c):
    return a <= b and b <= c

def mk(a, b, k):
    return [(a, b, k)]

def upd(d, a, b, k):
    if type(d[0]) == tuple:
        a0, b0, k0 = d[0]
        if between(a0, a, b0) or between(a0, b, b0) or between(a, a0, b):
            a = max(a, a0)
            b = min(b, b0)
            if a == a0 and b == b0:
                d[0] = (a, b, k0 + k)
            elif a == a0:
                d[0] = [mk(a0, b, k0 + k), mk(b + 1, b0, k0)]
            elif b == b0:
                d[0] = [mk(a0, b - 1, k0), mk(b, b0, k0 + k)]
            else:
                d[0] = [mk(a0, a - 1, k0), mk(a, b, k0 + k), mk(b + 1, b0, k0)]
    else:
        for ix in range(len(d[0])):
            upd(d[0][ix], a, b, k)

def mx(d):
    if type(d[0]) == tuple:
        return d[0][2]
    else:
        return max(map(mx, d[0]))

d = mk(1, n, 0)

for ix in range(m):
    a, b, k = map(int, sys.stdin.readline().split(' '))
    upd(d, a, b, k)

print mx(d)

