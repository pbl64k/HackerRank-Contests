import sys
import math

def nimber(x):
    if x == 0:
        return 0
    return int(math.log(x, 2)) + 1

def invn(x, m):
    if x == 0:
        return 0
    return min(int((2 ** x) - 1), m)

def v(x):
    if x == 0:
        return 0
    elif x % 2 == 1:
        return 1
    else:
        return 1 ^ nimber(x)

def f(x):
    if x >= 32768:
        return 32768
    elif x >= 128:
        return 128
    elif x >= 8:
        return 8
    else:
        return 2

def g(x):
    if x == 0:
        return 0
    elif x < 4 or x % 2 == 1:
        return 1
    else:
        k = f(x)
        r = k - invn(v(x) ^ nimber(k), k / 2)
        return r

for tix in range(int(sys.stdin.readline())):
    print g(int(sys.stdin.readline()))

