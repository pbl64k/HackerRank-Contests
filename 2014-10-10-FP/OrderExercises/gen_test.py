import random
import sys

n = int(sys.argv[1])

tst = []

for ix in range(n):
    x = 0
    while x == 0:
        x = random.randrange(-10000, 10001)
    tst.append(x)

print n, n
print ' '.join(map(str, tst))

