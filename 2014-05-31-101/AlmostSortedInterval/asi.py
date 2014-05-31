import sys

n = int(sys.stdin.readline())

xs = map(int, sys.stdin.readline().split(' '))

zs = list(zip(xs, range(n)))

zs.sort()

res = 0

t = [zs[0][1], None, None]
res += n - zs[0][1]

def find(t, pos, n):
    if t is None:
        return n
    if pos < t[0]:
        return find(t[1], pos, min(t[0], n))
    return find(t[2], pos, n)

def upd(t, pos):
    if t is None:
        return [pos, None, None]
    if pos < t[0]:
        t[1] = upd(t[1], pos)
        return t
    else:
        t[2] = upd(t[2], pos)
        return t

print res 
for ix in range(1, n):
    x, pos = zs[ix]
    bound = find(t, pos, n)
    print x, pos, bound, bound - pos
    res += bound - pos
    upd(t, pos)

print res

