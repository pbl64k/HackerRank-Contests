import sys

sys.setrecursionlimit(1000000)

n, m = map(int, sys.stdin.readline().split(' '))

def between(a, b, c):
    return a <= b and b <= c

def intersect(a1, b1, a2, b2):
    return between(a1, a2, b1) or between(a1, b2, b1) or between(a2, a1, b2)

class P(object):
    def __init__(self, a, b):
        self.a = a
        self.b = b
        self.m = self.a + (self.b - self.a + 1) / 2 - 1
        self.k = 0
        self.left = None
        self.right = None

    def l(self):
        assert self.a != self.b
        if self.left is None:
            self.left = P(self.a, self.m)
        return self.left

    def r(self):
        assert self.a != self.b
        if self.right is None:
            self.right = P(self.m + 1, self.b)
        return self.right

    def add(self, a, b, k):
        if intersect(self.a, self.b, a, b):
            aj = max(self.a, a)
            bj = min(self.b, b)
            if self.a == aj and self.b == bj:
                self.k += k
            elif self.a != self.b:
                self.l().add(a, b, k)
                self.r().add(a, b, k)

    def mx(self):
        r = [0]
        if self.left is not None:
            r.append(self.l().mx())
        if self.right is not None:
            r.append(self.r().mx())
        return max(r) + self.k

d = P(1, n)

for ix in range(m):
    a, b, k = map(int, sys.stdin.readline().split(' '))
    d.add(a, b, k)

print d.mx()

