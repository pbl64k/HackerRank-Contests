import sys

mm = 1000000007
nn = 314159

l = sys.stdin.readline()
if l[-1] == '\n':
    l = l[:-1]
a = [int(c) for c in l]
l = sys.stdin.readline()
if l[-1] == '\n':
    l = l[:-1]
b = [int(c) for c in l]

if len(a) > len(b):
    b = [0 for ix in range(len(a) - len(b))] + b
elif len(a) < len(b):
    a = [0 for ix in range(len(b) - len(a))] + a

p = [0 for ix in range(nn)]

a = p + a
b = p + b

z = 0
zs = []

for x in reversed(b):
    if x == 1:
        z += 1
    zs.append(z)

zs.reverse()

s = 0
f = 1

for ix in range(len(a) - 1, -1, -1):
    if a[ix] == 1:
        cnt = (nn + 1) - zs[ix]
    else:
        six = ix + nn + 1
        cnt = zs[ix] - (zs[six] if six < len(zs) else 0)
    #print cnt
    s = (s + ((f * cnt) % mm)) % mm
    f = (f * 2) % mm

print s

