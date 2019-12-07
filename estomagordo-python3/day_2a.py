import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve_part(d, noun, verb):
    d[1] = noun
    d[2] = verb

    p = 0

    while p < len(d):
        if d[p] == 99:
            break
        
        if d[p] == 1:
            d[d[p + 3]] = d[d[p + 1]] + d[d[p + 2]]
        elif d[p] == 2:
            d[d[p + 3]] = d[d[p + 1]] * d[d[p + 2]]
            
        p += 4

    return d[0]


def solve(d):
    return solve_part(d, 12, 2)
    

def read_and_solve():
    with open('input_2.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())