import sys

t = int(sys.stdin.readline())

def path(f, n, m, sn, sm, gn, gm):
    fr = []
    visited = [[False for mix in range(m)] for nix in range(n)]
    fr.append((sn, sm, 0))
    while len(fr) > 0:
        xn, xm, kk = fr.pop()
        if xn == gn and xm == gm:
            return kk
        visited[xn][xm] = True
        lst = []
        for dn, dm in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nn = xn + dn
            nm = xm + dm
            if nn >= 0 and nn < n and nm >= 0 and nm < m \
                    and not visited[nn][nm] and f[nn][nm]:
                lst.append((nn, nm, kk))
        dk = 0 if len(lst) == 1 else 1
        for nn, nm, kk in lst:
            fr.append((nn, nm, kk + dk))
    return None

for tix in range(t):
    n, m = map(int, sys.stdin.readline().split(' '))
    f = []
    for nix in range(n):
        l = sys.stdin.readline()
        l = l[:-1]
        mix = 0
        ll = []
        for c in l:
            if c == '.':
                ll.append(True)
            elif c == 'X':
                ll.append(False)
            elif c == 'M':
                startn = nix
                startm = mix
                ll.append(False)
            elif c == '*':
                goaln = nix
                goalm = mix
                ll.append(True)
            mix += 1
        f.append(ll)
    k = int(sys.stdin.readline())
    if path(f, n, m, startn, startm, goaln, goalm) == k:
        print 'Impressed'
    else:
        print 'Oops!'
