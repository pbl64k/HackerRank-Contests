import sys

sys.setrecursionlimit(1000000)

nm = {0: 0}

def mex(xs):
    xs = sorted(xs)
    r = 0
    for x in xs:
        if r < x:
            return r
        if r == x:
            r += 1
    return r

def posns(x):
    if x > 0:
        yield [x - 1]
    if x > 1:
        yield [x - 2]
    for l in range(1, ((x - 1) / 2) + 1):
        yield [l, x - l - 1]
    for l in range(1, ((x - 2) / 2) + 1):
        yield [l, x - l - 2]

def nimber(x):
    if x not in nm:
        nm[x] = mex(map(nimb, posns(x)))
    return nm[x]

def nimb(xs):
    return reduce(lambda x, y: x ^ y, map(nimber, xs))

print map(nimber, range(0, 6))
print nimb([1, 1])
print nimb([2])
print nimb([2, 2])
print nimb([5])
print nimb([297])
print nimb([298])
print nimb([299])
print nimb([300])

