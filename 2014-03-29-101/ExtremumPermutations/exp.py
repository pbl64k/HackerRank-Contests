M = 1000000007

memo_f = {}
def fact(n):
    if n not in memo_f:
        if n == 0:
            memo_f[n] = 1
        else:
            memo_f[n] = (n * fact(n - 1)) % M
    return memo_f[n]

memo_ch = {}
def ch(n, k):
    if (n, k) not in memo_ch:
        if k == 0 or k == n:
            memo_ch[(n, k)] = 1
        else:
            memo_ch[(n, k)] = (ch(n - 1, k - 1) + ch(n - 1, k)) % M
    return memo_ch[(n, k)]

def perms(n):
    if n == 1:
        yield [1]
    else:
        for p in perms(n - 1):
            for ix in range(0, n):
                yield p[0:ix] + [n] + p[ix:n]

def valid(p):
    gt = True
    for ix in range(len(p) - 1):
        if not ((gt and p[ix] > p[ix + 1]) or (not gt and p[ix] < p[ix + 1])):
            return False
        gt = not gt
    return True

for ix in range(1, 10):
    print fact(ix) - len(list(filter(valid, perms(ix))))
