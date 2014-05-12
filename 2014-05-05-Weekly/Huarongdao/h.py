import sys
import heapq

n, m, k, q = map(int, sys.stdin.readline().split(' '))

f = []
for ix in xrange(n):
    r = map(lambda x: x[0] == '1', sys.stdin.readline().split(' '))
    assert len(r) == m
    f.append(r)

def heur(pos, tx, ty):
    sx, sy, ex, ey = pos
    return abs(tx - sx) + abs(ty - sy)
    # TODO: empty?

def genns(pos):
    sx, sy, ex, ey = pos
    for dx, dy in ((-1, 0), (1, 0), (0, -1), (0, 1)):
        nx = ex + dx
        ny = ey + dy
        if nx < 0 or nx >= n or ny < 0 or ny >= m or not f[nx][ny]:
            continue
        if nx == sx and ny == sy:
            yield (1, (ex, ey, sx, sy))
        else:
            yield (1, (sx, sy, nx, ny))
    #def ff():
    #    for x in (-2, -1, 0, 1, 2):
    #        for y in (-2, -1, 0, 1, 2):
    #            if x == 0 and y == 0:
    #                continue
    #            yield x, y
    #for dx, dy in ff():
    for dx, dy in ((-1, 0), (1, 0), (0, -1), (0, 1)):
        nx = sx + dx
        ny = sy + dy
        if nx < 0 or nx >= n or ny < 0 or ny >= m or not f[nx][ny]:
            continue
        yield (k, (sx, sy, nx, ny))

def search(ex, ey, sx, sy, tx, ty):
    fr = []
    vis = set()
    pos = (sx, sy, ex, ey)
    c = heur(pos, tx, ty)
    #c = 0
    heapq.heappush(fr, (c, 0, pos))
    while len(fr) > 0:
        xx, mvs, pos = heapq.heappop(fr)
        if pos in vis:
            continue
        #print pos
        vis.add(pos)
        if pos[0] == tx and pos[1] == ty:
            return mvs
        for mc, posn in genns(pos):
            if posn in vis:
                continue
            c1 = mvs + mc
            heapq.heappush(fr, (c1 + heur(posn, tx, ty), c1, posn))
            # hmm...
            #heapq.heappush(fr, (c1, c1, posn))
    return -1

for ix in xrange(q):
    ex, ey, sx, sy, tx, ty = map(lambda x: int(x) - 1, sys.stdin.readline().split(' '))
    print search(ex, ey, sx, sy, tx, ty)

