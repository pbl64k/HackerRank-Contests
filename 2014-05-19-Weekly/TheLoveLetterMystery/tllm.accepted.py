import sys

def p(l):
    ll = len(l) / 2
    res = 0
    for ix in range(ll):
        res += abs(ord(l[ix]) - ord(l[-ix - 1]))
    return res

for tix in range(int(sys.stdin.readline())):
    l = sys.stdin.readline()
    if l[-1] == '\n':
        l = l[:-1]
    print p(l)
