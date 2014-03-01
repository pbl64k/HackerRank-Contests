import sys

n = int(sys.stdin.readline())
xs = map(int, sys.stdin.readline().split(' '))

xs.sort()

s = 0
ss = len(xs)
last = ss

for ix in range(ss - 1):
    ss -= 1
    if xs[ix] != xs[ix + 1]:
        print last
        last = ss

print last
