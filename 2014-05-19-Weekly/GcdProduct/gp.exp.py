def gcd(a, b):
    while b != 0:
        t = b
        b = a % b
        a = t
    return a

for a in range(1, 20):
    for b in range(1, 20):
        print gcd(a, b), '\t',
    print

