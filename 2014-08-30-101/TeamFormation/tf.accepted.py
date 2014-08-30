import sys

t = int(sys.stdin.readline())

for tix in range(t):
    line = sys.stdin.readline().split(' ')
    if line[-1] == '\n':
        line = line[:-1]
    xs = map(int, filter(lambda x: len(x) > 0, line))[1:]
    xs.sort()
    xxs = []
    last = None
    for x in xs:
        if last == x:
            xxs[-1] = (x, xxs[-1][1] + 1)
        else:
            last = x
            xxs.append((x, 1))
    seq = []
    lastsz = 0
    last = None
    res = len(xs)
    for x, sz in xxs:
        if last is None or last + 1 < x:
            for s in seq:
                res = min(res, len(s))
            seq = [[x] for ix in range(sz)]
            lastsz = sz
            last = x
        else:
            if sz < len(seq):
                for ix in range(len(seq) - sz):
                    s = seq.pop(0)
                    res = min(res, len(s))
            for s in seq:
                s.append(x)
            seq += [[x] for ix in range(sz - len(seq))]
            lastsz = sz
            last = x
    for s in seq:
        res = min(res, len(s))
    print res

