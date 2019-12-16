import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(nums):
    for _ in range(100):
        new_nums = []

        for i, _ in enumerate(nums):
            pattern = [0] * (i + 1)
            pattern += [1] * (i + 1)
            pattern += [0] * (i + 1)
            pattern += [-1] * (i + 1)

            tot = 0

            for j, num in enumerate(nums):
                tot += num * pattern[(j + 1) % len(pattern)]

            new_nums.append(abs(tot) % 10)

        nums = new_nums

    return ''.join(str(num) for num in nums[:8])
    

def read_and_solve():
    with open('input_16.txt') as f:
        data = list(map(int, list(f.readline().rstrip())))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())