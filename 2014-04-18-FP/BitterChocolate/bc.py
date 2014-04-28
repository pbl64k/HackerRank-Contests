
limit = 25

def genpos():
    for a in range(1, limit):
        for b in range(a + 1):
            for c in range(b + 1):
                yield (a, b, c)

losing = set([(1, 0, 0)])

def is_losing(pos):
    for ax in range(pos[2]):
        if (pos[0], pos[1], ax) in losing:
            return False
    for ax in range(pos[1]):
        if (pos[0], ax, min(ax, pos[2])) in losing:
            return False
    for ax in range(pos[0]):
        if (ax, min(ax, pos[1]), min(ax, pos[2])) in losing:
            return False
    return True

for pos in genpos():
    print pos
    if pos in losing:
        continue
    if is_losing(pos):
        losing.add(pos)

for pos in losing:
    print pos

print (1, 1, 1) in losing
print (2, 2, 1) in losing

