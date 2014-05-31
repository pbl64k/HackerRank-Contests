import sys

sys.stdin.readline()

xs = map(int, sys.stdin.readline().split(' '))

def asi(xs, a, b, lx, rx):
    if a + 1 == b:
        return 1, (1 if xs[a] > lx else 0), (1 if xs[a] < rx else 0)
    m = (a + b) / 2
    t0, l0, r0 = asi(xs, a, m, lx, xs[m])
    t1, l1, r1 = asi(xs, m, b, xs[m - 1], rx)
    l = l0 + (l1 if l0 == m - a else 0)
    r = r1 + (r0 if r1 == b - m else 0)
    return t0 + t1 + (r0 * l1), l, r

t, l, r = asi(xs, 0, len(xs), 0, len(xs) + 1)
print t

