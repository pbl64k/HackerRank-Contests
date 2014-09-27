import sys

n = int(sys.stdin.readline())

ords = []

for ix in range(n):
    xs = map(int, sys.stdin.readline().split(' '))
    t, d = xs[0], xs[1]
    ords.append((t + d, ix + 1))

ords.sort()

print ' '.join(map(lambda x: str(x[1]), ords))

