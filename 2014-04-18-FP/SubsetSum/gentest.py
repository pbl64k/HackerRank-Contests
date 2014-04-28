
import random

n = 100000

print n

xs = []

for ix in range(n):
    xs.append(random.randrange(1, 1000000001))

print ' '.join(map(str, xs))

t = 100000

print t

for ix in range(t):
    print int(random.uniform(1.0, 1e9 * 1e5))

