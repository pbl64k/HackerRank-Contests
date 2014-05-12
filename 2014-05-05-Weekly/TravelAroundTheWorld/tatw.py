import sys

n, c = map(int, sys.stdin.readline().split(' '))
a = map(int, sys.stdin.readline().split(' '))
b = map(int, sys.stdin.readline().split(' '))

excess = [min(x, c) - y for x, y in zip(a, b)]

sols = []
lix = n - 1
rix = 0

while lix >= rix:
    exc = excess[rix]
    if exc >= 0:
        sols.append(exc)
    else:
        while len(sols) > 0 or lix > rix:
            if len(sols) > 0:
                c = sols.pop()
                exc += c
            else:
                c = excess[lix]
                lix -= 1
                lix %= n
                exc += c
            if exc >= 0:
                sols.append(exc)
                break
    rix += 1

print len(sols)

