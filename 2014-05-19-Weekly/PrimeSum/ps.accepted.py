import sys
import math
import random

def g(n):
    yield 2
    l = math.ceil(math.sqrt(n))
    r = 3
    while r <= l:
        yield r
        r += 2

def primep_naif(n):
    for x in g(n):
        if n % x == 0:
            return False
    return True

def dec(n):
    s = 0
    d = n
    while d % 2 == 0:
        d /= 2
        s += 1
    return s, d

def expmod(b, e, m):
    r = 1
    b = b % m
    while e > 0:
        if e % 2 == 1:
            r = (r * b) % m
        e >>= 1
        b = (b * b) % m
    return r

def primep0(n, k = 50):
    s, d = dec(n - 1)
    for ix in xrange(k):
        a = random.randrange(2, n - 1)
        x = expmod(a, d, n)
        if x == 1 or x == n - 1:
            continue
        cont = False
        for jx in xrange(s - 1):
            x = expmod(x, 2, n)
            if x == 1:
                return False
            elif x == n - 1:
                cont = True
                break
        if cont:
            continue
        return False
    return True
    
def primep(n):
    if n == 2 or n == 3:
        return True
    elif n > 3:
        if n % 2 == 1:
            return primep0(n)
        else:
            return False
    else:
        return False

def sump(n, k):
    if n < 2:
        return False
    elif k == 1:
        return primep(n)
    elif n % 2 == 0: # even
        if k == 2:
            if n >= 4:
                return True
            else:
                return False
        else:
            return sump(n - ((k - 2) * 2), 2)
    else: # odd
        if k == 2:
            return primep(n - 2)
        elif k == 3:
            if n >= 7:
                return True
            else:
                return False
        else:
            return sump(n - ((k - 3) * 2), 3)
        
for t in range(int(sys.stdin.readline())):
    n, k = map(int, sys.stdin.readline().split(' '))
    print 'Yes' if sump(n, k) else 'No'

#for k in range(10000):
#    if k % 100 == 0:
#        print k
#    x = random.randrange(1, 100000000000)
#    a = primep(x)
#    b = primep_naif(x)
#    if a != b:
#        print x, a, b

