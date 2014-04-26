#!/bin/python

# Complete the function below.

def maxXor(l, r):
    return max([a ^ b for a in range(l, r + 1) for b in range(a, r + 1)])    

_l = int(raw_input());


_r = int(raw_input());

res = maxXor(_l, _r);
print(res)


