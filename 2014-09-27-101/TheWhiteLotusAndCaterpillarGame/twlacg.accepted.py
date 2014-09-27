import sys

n, m = map(int, sys.stdin.readline().split(' '))

r = 0.0

for l in range(m):
    rr = n - 1.0
    if l >= n:
        rr += (float(l - n + 1)) * (float(l - 1) / float(m))
    if l < m - n:
        rr += (float(m - n - l)) * (float(m - l - 2) / float(m))
    r += rr / float(m)

print r

