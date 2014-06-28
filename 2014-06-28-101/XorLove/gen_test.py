import random

n = 100
m = 100

print n

xs = [random.randrange(1, 1000000) for ix in range(n)]

print ' '.join(map(str, xs))

print m

for ix in range(m):
    k = random.randrange(0, 1000000)
    p = random.randrange(1, n + 1)
    r = random.randrange(1, n + 1)
    print k, min(p, r), max(p, r)

