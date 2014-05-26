import sys

m = 1000000007

s = sys.stdin.readline()

if s[-1] == '\n':
    s = s[:-1]

n = len(s)

f = n
c = 1
k = 1

r = 0

for ix in range(1, n + 1):
    jx = -ix
    r = (r + ((((f * c) % m) * int(s[jx])) % m)) % m
    f -= 1
    k = (k * 10) % m
    c = (c + k) % m

print r

