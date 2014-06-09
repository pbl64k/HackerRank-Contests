import sys

t = int(sys.stdin.readline())

def f(xs):
    n = len(xs)
    m = n / 2
    for ix in range(m):
        if xs[ix] != xs[n - ix - 1]:
            if xs[ix + 1] == xs[n - ix - 1]:
                return ix
            else:
                return n - ix - 1
    return m

for tix in range(t):
    l = sys.stdin.readline()
    if l[-1] == '\n':
        l = l[:-1]
    print f(l)

