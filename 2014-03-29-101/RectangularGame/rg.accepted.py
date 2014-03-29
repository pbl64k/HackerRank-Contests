import sys

n = int(sys.stdin.readline())

x = None
y = None

for ix in range(n):
    a, b = map(int, sys.stdin.readline().split(' '))
    if x is None or a < x:
        x = a
    if y is None or b < y:
        y = b

print x * y
