import sys

sys.setrecursionlimit(100000)

n, m, root = map(int, sys.stdin.readline().split(' '))

children = [[] for ix in range(n)]
cs = []

for ix in range(n - 1):
    h, t = map(lambda x: int(x) - 1, sys.stdin.readline().split(' '))
    children[h].append(t)
    children[t].append(h)

for ix in range(n):
    c = int(sys.stdin.readline())
    cs.append(c)

cns = [None for ix in range(n)]

#def bt(n, visited = set()):
#    r = set([cs[n]])
#    visited.add(n)
#    for cix in children[n]:
#        if cix not in visited:
#            r.update(bt(cix, visited))
#    cns[n] = len(r)
#    return r

def bt(n):
    fr = [(n, True, None, None)]
    visited = set()
    r = set()
    while len(fr) > 0:
        n, f, p, s = fr.pop()
        if f:
            r = set()
            visited.add(n)
            lst = []
            for cix in children[n]:
                if cix not in visited:
                    lst.append(cix)
            fr.append((n, False, lst, set([cs[n]])))
        else:
            if len(p) == 0:
                s.update(r)
                cns[n] = len(s)
                r = s
            else:
                ix = p.pop()
                s.update(r)
                fr.append((n, False, p, s))
                fr.append((ix, True, None, None))

bt(root - 1)

for ix in range(m):
    q = int(sys.stdin.readline()) - 1
    print cns[q]
