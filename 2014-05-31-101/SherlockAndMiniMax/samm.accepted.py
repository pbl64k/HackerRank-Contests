import sys

n = int(sys.stdin.readline())
xs = map(int, sys.stdin.readline().split(' '))
p, q = map(int, sys.stdin.readline().split(' '))

xs.sort()

best = None
bestd = None

if p < xs[0]:
    best = p
    bestd = xs[0] - p

for a, b in zip(xs, xs[1:]):
    if q < a:
        break
    if b < p:
        continue
    m = (a + b) / 2
    if q < m:
        m = q
    elif m <p:
        m = p
    d = min(m - a, b - m)
    if best is None or d > bestd:
        best = m
        bestd = d

if xs[-1] < q:
    m = q
    d = q - xs[-1]
    if best is None or d > bestd:
        best = m
        bestd = d

print best
    
