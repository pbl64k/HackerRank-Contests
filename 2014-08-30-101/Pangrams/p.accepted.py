import sys

s = sys.stdin.readline()

cs = set()

for c in s:
    if c == ' ' or c == '\n':
        continue
    cs.add(c.lower())

if len(cs) == 26:
    print 'pangram'
else:
    print 'not pangram'

