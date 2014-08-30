import sys

t = int(sys.stdin.readline())

r2s = {}

def r2sq(x):
    if x not in r2s:
        num = 0
        a = int(x ** 0.5 + 1)
        b = 0
        while a >= b:
            while True:
                s = a ** 2 + b ** 2
                if s <= x:
                    if s == x:
                        if a == b or b == 0:
                            num += 1
                        else:
                            num += 2
                    b += 1
                else:
                    break
            a -= 1
        r2s[x] = 4 * num
    return r2s[x]

for tix in range(t):
    xs = map(int, sys.stdin.readline().split(' ')[:2])
    rsq = xs[0]
    k = xs[1]
    if k >= r2sq(rsq):
        print 'possible'
    else:
        print 'impossible'

