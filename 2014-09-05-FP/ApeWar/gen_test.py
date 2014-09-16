import sys
import random

sz = int(sys.argv[1])

print sz, sz

ps = []

for ix in range(2, sz + 1):
    #ps.append(random.randrange(1, ix))
    ps.append(ix - 1)

print ' '.join(map(str, ps))

for ix in range(sz):
    x = random.randrange(1, sz + 1)
    y = random.randrange(1, sz + 1)
    p = random.randrange(1, sz + 1)
    q = random.randrange(1, sz + 1)
    print x, y, min(p, q), max(p, q)

