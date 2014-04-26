import sys

sys.stdin.readline()

xs = map(int, sys.stdin.readline().split(' '))

n = len(xs)
ss = sum(xs)

s = 0

for ix in range(n):
    s += (ix + 1) * xs[ix]

b = s

for x in xs:
    s = s - ss + (n * x)
    if s > b:
        b = s

print b
