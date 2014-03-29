import copy
import sys

t = int(sys.stdin.readline())

def sat(f, rs, n):
    if len(rs) == 0:
        return True
    x, y = rs.pop(0)
    for dx, dy in ((0, 1), (1, 0), (1, -1)):
        x2 = x + dx
        y2 = y + dy
        if x2 == n or y2 < 0 or y2 > 1 or f[y2][x2]:
            continue
        f0 = copy.deepcopy(f)
        f0[y][x] = True
        f0[y2][x2] = True
        rs0 = filter(lambda x: x[0] != x2 or x[1] != y2, rs)
        if sat(f0, rs0, n):
            return True
    return False

for tix in range(t):
    n = int(sys.stdin.readline())
    l1 = sys.stdin.readline()
    l2 = sys.stdin.readline()
    f = []
    l1 = filter(lambda x: x == '0' or x == '1', l1)
    l2 = filter(lambda x: x == '0' or x == '1', l2)
    f.append(map(lambda x: True if x == '1' else False, l1))
    f.append(map(lambda x: True if x == '1' else False, l2))
    coords = [(x, y) for x in range(n) for y in (0, 1)]
    rs = filter(lambda x: not f[x[1]][x[0]], coords)
    print 'YES' if sat(f, rs, n) else 'NO'

