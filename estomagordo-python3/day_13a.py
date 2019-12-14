import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def solve(d):
    computer = Computer(d, 0)
    count = 0
    done = True
    loops = 0
    
    while done:
        done, retval = computer.get_output()
        if loops % 3 == 2 and retval == 2:
            count += 1
        loops += 1

    return count
    

def read_and_solve():
    with open('input_13.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())