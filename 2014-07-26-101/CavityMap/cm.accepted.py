import sys

n = int(sys.stdin.readline())

m = []

for ix in range(n):
    l = sys.stdin.readline()
    if l[-1] == '\n':
        l = l[:-1]
    m.append([int(c) for c in l])

m2 = [[c for c in l] for l in m]

ds = [(-1, 0), (1, 0), (0, -1), (0, 1)]

for x in range(1, n - 1):
    for y in range(1, n - 1):
        v = m[x][y]
        if all(map(lambda z: m[x + z[0]][y + z[1]] < v, ds)):
            m2[x][y] = 'X'

for l in m2:
    print ''.join(map(str, l))
        
