from heapq import *
import sys

M = 1000000007

def readints(): return map(int, sys.stdin.readline().split(' '))

n, m = readints()
a = readints()
b = readints()
c = readints()

mps = {}

for x, y in zip(b, c):
    if x not in mps:
        mps[x] = y
    else:
        mps[x] = (mps[x] * y) % M

q = []

for k in mps:
    heappush(q, (k, k, mps[k]))

while len(q) > 0:
    ix, k, y = heappop(q)
    if ix > n:
        break
    a[ix - 1] = (a[ix - 1] * y) % M
    heappush(q, (ix + k, k, y))

print ' '.join(map(str, a))

