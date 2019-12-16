import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(nums):
    offset = int(''.join(str(num) for num in nums[:7]))
    nums *= 10000
    
    nums = nums[offset:]
    
    for _ in range(100):
        new_nums = [sum(abs(x) for x in nums)]

        for x in range(1, len(nums)):
            new_nums.append(new_nums[-1] - nums[x - 1])

        nums = list(map(lambda x: x % 10, new_nums))

    
    return int(''.join(str(num) for num in nums[:8]))
    

def read_and_solve():
    with open('input_16.txt') as f:
        data = list(map(int, list(f.readline().rstrip())))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())