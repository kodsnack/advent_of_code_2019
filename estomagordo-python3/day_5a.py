import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve_part(d, noun, verb):
    # d[1] = noun
    # d[2] = verb
    inp = 1
    p = 0
    steps = 0

    while p < len(d):
        steps += 1
        if d[p] % 100 == 99:
            break
        
        if d[p] % 100 == 1:
            a = d[p + 1] if len(str(d[p])) > 2 and str(d[p])[-3] == '1' else d[d[p + 1]]
            b = d[p + 2] if len(str(d[p])) > 3 and str(d[p])[-4] == '1' else d[d[p + 2]]
            d[d[p + 3]] = a + b
            p += 4
        elif d[p] % 100 == 2:
            # a = d[p + 1] if ((d[p] % 1000) // 100) == 1 else d[d[p + 1]]
            # b = d[p + 2] if (((d[p] % 10000) // 1000) == 1) else d[d[p + 2]]
            a = d[p + 1] if len(str(d[p])) > 2 and str(d[p])[-3] == '1' else d[d[p + 1]]
            b = d[p + 2] if len(str(d[p])) > 3 and str(d[p])[-4] == '1' else d[d[p + 2]]
            d[d[p + 3]] = a * b
            p += 4
        elif d[p] % 100 == 3:
            d[d[p + 1]] = inp
            p += 2
        elif d[p] % 100 == 4:
            print(d[d[p + 1]])
            p += 2
            
       

    return d[0]


def solve(d):
    return solve_part(d, 12, 2)
    

def read_and_solve():
    with open('input_5.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())