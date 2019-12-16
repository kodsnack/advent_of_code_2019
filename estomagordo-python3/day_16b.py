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


def get_vital_positions(numlen, j):
    pattern = [patternize(i, j) for i in range(numlen)]

    return [x for x in range(numlen) if pattern[x] == 1], [x for x in range(numlen) if pattern[x] == -1]

def solve(nums):
    offset = int(''.join(str(num) for num in nums[:7]))
    nums *= 10000
    
    nums = nums[offset:]
    
    for run in range(100):
        new_nums = [sum(abs(x) for x in nums)]

        for x in range(1, len(nums)):
            new_nums.append(new_nums[-1] - nums[x - 1])

        nums = list(map(lambda x: x % 10, new_nums))

    
    return ''.join(str(num) for num in nums[:8])
    

def read_and_solve():
    with open('input_16.txt') as f:
        data = list(map(int, list(f.readline().rstrip())))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())