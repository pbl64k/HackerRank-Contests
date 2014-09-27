import sys

sys.stdin.readline()

hs = map(int, sys.stdin.readline().split(' '))

r = 0
t = None

def bork(t, h):
    if t is None:
        return 0, (h, 1, None, None)
    h0, n, l, r = t
    if h == h0:
        return n, (h0, n + 1, None, r)
    elif h < h0:
        res, l0 = bork(l, h)
        return res, (h0, n, l0, r)
    else:
        res, t0 = bork(r, h)
        return res, t0

for h in hs:
    l, t = bork(t, h)
    r += 2 * l

print r

