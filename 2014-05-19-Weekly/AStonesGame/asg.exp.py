import sys
import math

nm = {0: 0}

def mex(xs):
    xs = sorted(xs)
    r = 0
    for x in xs:
        if r < x:
            return r
        if r == x:
            r += 1
    return r

def posns(x):
    return xrange(0, (x / 2) + 1)

def nimber0(x):
    if x not in nm:
        nm[x] = mex(map(nimber0, posns(x)))
    return nm[x]

def nimber(x):
    if x == 0:
        return 0
    return int(math.log(x, 2)) + 1

#for x in range(1, 1000000000):
#    if x % 1000 == 0:
#        print x, nimber(x), nimber0(x)
#    if nimber(x) != nimber0(x):
#        print x, nimber(x), nimber0(x)
#        break
#exit()

def v0(x):
    return reduce(lambda x, y: x ^ y, map(nimber, range(1, x + 1)))

def v(x):
    if x == 0:
        return 0
    elif x % 2 == 1:
        return 1
    else:
        return 1 ^ nimber(x)
        #n = x / 2
        #l = int(math.log(n, 2)) + 2
        #if l % 2 == 0:
        #    return l + 1
        #else:
        #    return l - 1

#for x in range(1, 1000000000):
#    if x % 1000 == 0:
#        print x, v(x), v0(x)
#    if v(x) != v0(x):
#        print x, v(x), v0(x)
#        break
#exit()

def invn(x, m):
    if x == 0:
        return 0
    #return min(int(math.log(x, 2)) + 1, m)
    return min(int((2 ** x) - 1), m)

#for x in range(100):
#    print x, nimber(x), invn(nimber(x), 1000000000000000)
#    #print invn(x, 1000000)
#exit()

#def ffff(x):
#    if x % 2 == 1:
#        return 1
#    elif x >= 34359738368: return 2147483645
#    elif x >= 8589934592: return 2147483646
#    elif x >= 4294967296: return 2147483648
#    elif x >= 2147483648: return 2147483647
#    elif x >= 8388608: return 32764
#    elif x >= 524288: return 32765
#    elif x >= 131072: return 32766
#    elif x >= 65536: return 32768
#    elif x >= 32768: return 32767
#    elif x >= 2048: return 125
#    elif x >= 512: return 126
#    elif x >= 256: return 128
#    elif x >= 128: return 127
#    elif x >= 32: return 6
#    elif x >= 16: return 8
#    elif x >= 8: return 7
#    elif x >= 4: return 2
#    else: return 1

def fff(x):
    if x >= 32768:
        return 32768
    elif x >= 128:
        return 128
    elif x >= 8:
        return 8
    else:
        return 2

def ffff(x):
    if x == 0:
        return 0
    elif x < 4 or x % 2 == 1:
        return 1
    else:
        k = fff(x)
        #print k, v(x), nimber(k), v(x) ^ nimber(k), invn(v(x) ^ nimber(k), x / 2)
        r = k - invn(v(x) ^ nimber(k), k / 2)
        return r

oldmm = None
lkexp = 0
#for n in range(4, 11, 1):
#for n in range(1, 1000000001, 1):
#for n in range(1, 11, 1):
#for nexp in range(0, int(math.log(1000000000000001, 2)) + 1):
for n in range(2, 1000000001, 2):
    #n = 2 ** nexp
    #print n
    vv = v(n)
    #print n, 'nimber:', vv, bin(vv)[1:]
    mm = None
    kkk = None
    #for k in xrange(1, n + 1):
    #for kexp in range(lkexp, int(math.log(n, 2)) + 1):
    for kexp in range(0, int(math.log(n, 2)) + 1):
        k = 2 ** kexp
        if vv ^ nimber(k) <= nimber(k):
            #print vv, nimber(k), vv ^ nimber(k)
            f = k - invn(vv ^ nimber(k), k / 2)
            #print '   ', k, f, 'k-nimber:', nimber(k), bin(nimber(k))[1:], 'xor:', vv ^ nimber(k), bin(vv ^ nimber(k))[1:]
            #print f
            if mm is None or f < mm:
                mm = f
                kkk = k
                #print 'BOING!'
                #lkexp = kexp
                break
    if n % 25000000 == 0:
        print n
    #if n % 10000 == 0:
    #    print n
    #if mm != oldmm:
    if mm != ffff(n):
    #if True:
        print n, 'ff:', mm, kkk
        print mm, ffff(n)
        oldmm = mm
        break
    #print '*****'
    
