import sys
import math

t = int(sys.stdin.readline())

for ix in range(t):
    a, b, x = map(int, sys.stdin.readline().split(' '))
    c = a ** b
    m = int(math.floor(c / x))
    x1 = m * x
    x2 = x1 + x
    d1 = c - x1
    d2 = x2 - c
    if d1 <= d2:
        print x1
    else:
        print x2

