import sys

def inpint(x): return int(sys.stdin.readline())

t = inpint('')

for tix in xrange(t):
    n, x, y = map(inpint, range(3))
    a = min(x, y)
    b = max(x, y)
    print ' '.join(map(lambda x: str(b * x + a * (n - x - 1)), range(n)))

