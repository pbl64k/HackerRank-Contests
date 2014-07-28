import random

n = 100000
p = 0.005

print n

for x in range(1, n + 1):
    if x > 1 and random.uniform(0.0, 1.0) <= p:
        print -random.randrange(1, x)
    else:
        print random.randrange(1, 1000000001)

