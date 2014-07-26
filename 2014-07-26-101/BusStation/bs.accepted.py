import sys

sys.stdin.readline()

xs = map(int, sys.stdin.readline().split(' '))

res = []
t = 0

s1 = set()
s2 = set()
ss1 = 0
ss2 = 0

s = sum(xs)

for ix in range(len(xs)):
    ss1 += xs[ix]
    ss2 += xs[len(xs) - ix - 1]
    if s % ss1 == 0:
        s1.add(ss1)
    if s % ss2 == 0:
        s2.add(ss2)

s1 &= s2

for x in xs:
    t += x
    if t in s1:
        res0 = [(t, 0)]
    else:
        res0 = []
    for mx, c in res:
        if c + x <= mx:
            res0.append((mx, (c + x) % mx))
    res = res0

rs = filter(lambda x: x[1] == 0, res)
rrs = map(lambda x: x[0], rs)

rrs.sort()

print ' '.join(map(str, rrs))

