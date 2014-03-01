import sys
from functools import *

a = sys.stdin.readline()
b = sys.stdin.readline()

x = {}

def f(w, cf = 1):
    for c in w:
        if c not in 'abcdefghijklmnopqrstuvwxyz':
            continue
        if c not in x:
            x[c] = 0
        x[c] += cf

f(a)
f(b, -1)

res = reduce(lambda a, b: a + abs(x[b]), x.keys(), 0)

print(res)
