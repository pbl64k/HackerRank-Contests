import sys

t = int(sys.stdin.readline())

def f(xs):
    r = {}
    for x in xs:
        if x not in r:
            r[x] = 0
        r[x] += 1
    a = 0
    for k in r:
        n = r[k]
        a += n ** 2 - n
    return a

for tix in range(t):
    n = int(sys.stdin.readline())
    xs = map(int, sys.stdin.readline().split(' '))
    print f(xs)
