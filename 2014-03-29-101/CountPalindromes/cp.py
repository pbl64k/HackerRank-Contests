import math
import sys

def tri(n):
    return n * (n + 1) / 2

def inv_tri(x):
    return (-1.0 + (1.0 + 8.0 * x) ** 0.5) / 2.0

def minp(x):
    z = int(math.floor(inv_tri(x)))
    if tri(z) == x:
        return z
    return z + minp(x - tri(z))

t = int(sys.stdin.readline())

for tix in range(t):
    x = int(sys.stdin.readline())
    print minp(x)

