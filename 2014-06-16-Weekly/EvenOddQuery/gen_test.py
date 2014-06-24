import random

n = 50

print n

xs = []

for ix in range(n):
    xs.append(random.randrange(10))

print ' '.join(map(str, xs))

print n

for ix in range(n):
    x = random.randrange(1, n + 1)
    y = random.randrange(1, n + 1)
    print min(x, y), max(x, y)

