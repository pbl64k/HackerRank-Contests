import sys

sys.setrecursionlimit(1000000)

n = int(sys.stdin.readline())

xs = map(int, sys.stdin.readline().split(' '))

#def find(x, y):
#    if x > y:
#        return 1
#    return xs[x] ** find(x + 1, y)

def find(x, y):
    if x > y:
        return 1
    if xs[x] % 2 == 1:
        return xs[x]
    if find(x + 1, y) == 0:
        return 1
    return xs[x]

q = int(sys.stdin.readline())

for ix in range(q):
    x, y = map(lambda x: int(x) - 1, sys.stdin.readline().split(' '))
    print 'Even' if find(x, y) % 2 == 0 else 'Odd'

