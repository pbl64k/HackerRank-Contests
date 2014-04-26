import sys

t = int(sys.stdin.readline())

def solve(xs):
    xn = len(xs)
    ss = [(0, 0)]
    for ix in range(xn):
        eix = xn - ix - 1
        best = None
        for dx in (1, 2, 3):
            if eix + dx > xn:
                continue
            p = sum(xs[eix:eix + dx])
            a, b = ss[ix + 1 - dx]
            r = (b + p, a)
            if best is None or best[0] < r[0]:
                best = r
        ss.append(best)
    return ss[-1][0]

for tix in range(t):
    sys.stdin.readline()
    xs = map(int, sys.stdin.readline().split(' '))
    print solve(xs)

