import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def patternize(i, j):
    if i == 0 and j == 0:
        return 1

    j += 1

    width = i + 1

    band = j // width

    return [0, 1, 0, -1][band % 4]


def solve(nums):
    for _ in range(100):
        new_nums = []

        for i, _ in enumerate(nums):
            tot = 0

            for j, num in enumerate(nums):
                tot += num * patternize(i, j)

            new_nums.append(abs(tot) % 10)

        nums = new_nums

    return int(''.join(str(num) for num in nums[:8]))
    

def read_and_solve():
    with open('input_16.txt') as f:
        data = list(map(int, list(f.readline().rstrip())))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())