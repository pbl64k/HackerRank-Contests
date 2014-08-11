import sys

t = int(sys.stdin.readline())

def bt(x):
    res = []
    while x > 0:
        res.append(x % 2)
        x /= 2
    res.reverse()
    return res

def f(n, x, g):
    if x == [1]:
        return not g
    if n == 1:
        x.pop()
        return f(n, x, not g)
    else:
        x.pop(0)
        while x[0] == 0:
            x.pop(0)
        return f(n - 1, x, not g)

for tix in range(t):
    x = bt(int(sys.stdin.readline()))
    print 'Louise' if f(sum(x), x, True) else 'Richard'

