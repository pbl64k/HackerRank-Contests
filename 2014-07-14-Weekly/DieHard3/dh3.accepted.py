import sys

t = int(sys.stdin.readline())

def ns(n, m, a, b):
    if n == 0:
        yield a, m
    else:
        yield 0, m
        if m < b:
            d = min(b - m, n)
            yield n - d, m + d
    if m == 0:
        yield n, b
    else:
        yield n, 0
        if n < a:
            d = min(a - n, m)
            yield n + d, m - d

def solve(a, b, c):
    q = [(0, 0)]
    v = set()
    while len(q) > 0:
        n, m = q.pop()
        if n == c or m == c:
            return 'YES'
        if (n, m) in v:
            continue
        v.add((n, m))
        for n0, m0 in ns(n, m, a, b):
            if (n0, m0) in v:
                continue
            q.append((n0, m0))
    return 'NO'

for tix in range(t):
    a, b, c = map(int, sys.stdin.readline().split(' '))
    print solve(a, b, c)

