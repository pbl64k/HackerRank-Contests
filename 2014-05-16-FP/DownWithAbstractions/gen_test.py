import random

num = 1
sz0 = 20000

def gen(sz, vrs):
    if sz == 1:
        return random.choice(list(vrs))
    else:
        if random.uniform(0.0, 1.0) < 0.5:
            var = 'x_' + str(random.randrange(sz0 / 3))
            return '(\\' + var + '. ' + gen(sz - 1, vrs | set([var])) + ')'
        else:
            split = random.randrange(1, sz)
            return '(' + gen(split, vrs) + ' ' + gen(sz - split, vrs) + ')'

print num

for ix in range(num):
    print '(\\x_0. ' + gen(sz0, set(['x_0'])) + ')'

