import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(d):
	fuel = 0

	for x in d:
		while x // 3 - 2 > 0:
			x = x // 3 - 2
			fuel += x

	return fuel
	

def read_and_solve():
	with open('input_1.txt') as f:
		data = [int(line.rstrip()) for line in f]
		return solve(data)

if __name__ == '__main__':
	print(read_and_solve())