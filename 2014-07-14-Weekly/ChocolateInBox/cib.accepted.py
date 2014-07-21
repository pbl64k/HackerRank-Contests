import sys

sys.stdin.readline()

xs = map(int, sys.stdin.readline().split(' '))

s = reduce(lambda a, b: a ^ b, xs, 0)

r = 0

for x in xs:
    if x ^ s < x:
        r += 1

print r

