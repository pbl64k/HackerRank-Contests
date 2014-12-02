import sys

n = int(sys.stdin.readline())

for tix in range(n):
    a = sys.stdin.readline()
    if a[-1] == '\n': a = a[:-1]
    b = sys.stdin.readline()
    if b[-1] == '\n': b = b[:-1]
    aset = set([c for c in a])
    bset = set([c for c in b])
    if len(aset & bset) > 0:
        print 'YES'
    else:
        print 'NO'

