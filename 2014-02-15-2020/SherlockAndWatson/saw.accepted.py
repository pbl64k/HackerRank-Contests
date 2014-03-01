import sys

n, k, q = map(int, sys.stdin.readline().split(' '))

v = map(int, sys.stdin.readline().split(' '))

for jx in range(q):
    ix = int(sys.stdin.readline())
    print v[(ix - k) % n]
