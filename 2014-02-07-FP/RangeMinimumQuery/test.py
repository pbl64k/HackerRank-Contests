
import random

nums = 100000
qs = 100000

print nums, qs

print ' '.join(map(str, [random.randrange(-100000, 100000) for ix in range(nums)]))

for ix in range(qs):
	a = random.randrange(nums)
	b = random.randrange(nums)
	if b < a:
		a, b = b, a
	print a, b

