import sys

t = int(sys.stdin.readline())

for tix in range(t):
    sys.stdin.readline()
    xs = map(int, sys.stdin.readline().split(' '))
    a = 0
    b = sum(xs) - xs[0]
    found = False
    if a != b:
        for ix in range(len(xs) - 1):
            a += xs[ix]
            b -= xs[ix + 1]
            if a == b:
                found = True
                break
    else:
        found = True
    if found:
        print "YES"
    else:
        print "NO"

