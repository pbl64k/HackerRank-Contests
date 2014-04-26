import sys

t = int(sys.stdin.readline())

def slv(a, b, xa, xb, la, lb):
    while xa < la or xb < lb:
        ca = 'z'
        if xa < la:
            ca = a[xa]
        cb = 'z'
        if xb < lb:
            cb = b[xb]
        if ca < cb:
            return 0
        if cb < ca:
            return 1
        xa += 1
        xb += 1
    return 0

def solve(a, b):
    la = len(a)
    lb = len(b)
    xa = 0
    xb = 0
    res = []
    lr = None
    while xa < la or xb < lb:
        if xa == la:
            res.append(b[xb])
            xb += 1
        elif xb == lb:
            res.append(a[xa])
            xa += 1
        elif a[xa] == b[xb]:
            if lr is None:
                lr = slv(a, b, xa, xb, la, lb)
            if lr == 0:
                res.append(a[xa])
                xa += 1
            else:
                res.append(b[xb])
                xb += 1
        elif a[xa] < b[xb]:
            res.append(a[xa])
            xa += 1
            lr = None
        else:
            res.append(b[xb])
            xb += 1
            lr = None
    return ''.join(res)
    
for tix in range(t):
    a = sys.stdin.readline()[:-1]
    b = sys.stdin.readline()
    if b[-1] == '\n':
        b = b[:-1]
    print solve(a, b)

