import sys

t = int(sys.stdin.readline())

def f(n, v):
    r = 0
    if n % 3 == 0:
        for nix in range(n / 3):
            a = nix + n / 3
            b = nix + 2 * n / 3
            if a == b:
                continue
            if v[nix] == v[a] and v[nix] == v[b]:
                r += 1
    for nix in range(n):
        for dx in range(1, n / 2):
            a = (nix + dx) % n
            b = (nix - dx) % n
            if n % 3 == 0 and dx == n / 3:
                continue
            if a == b:
                continue
            if v[nix] == v[a] and v[nix] == v[b]:
                r += 1
    return r

for tix in range(t):
    l = sys.stdin.readline()
    l = l[:-1]
    v = []
    for c in l:
        v.append(int(c))
    n = len(v)
    print 'Case %d: %d' % (tix + 1, f(n, v))
