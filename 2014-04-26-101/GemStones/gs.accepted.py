import sys

t = int(sys.stdin.readline())

gs = None

for tix in range(t):
    s = sys.stdin.readline()[:-1]
    ss = set()
    for c in s:
        ss.add(c)
    if gs is None:
        gs = ss
    else:
        gs &= ss

print len(gs)
