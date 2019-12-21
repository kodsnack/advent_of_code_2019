import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def solve(d):
    # instructions =
    # [
    #     'AND A T', 
    #     'AND B T', 
    #     'AND C T', 
    #     'AND D T', 
    #     'AND T T', 
    #     'AND J T', 
    # ]
    
    inp = """OR D J
OR A T
AND B T
AND C T
NOT T T
AND T J
WALK
"""
    p = 0
    steps = 0

    computer = Computer(d, ord(inp[p]))

    while True:
        steps += 1
        retcode, retval = computer.step()

        if retcode == -1:
            break
        
        if retcode == 0 and retval == 3:
            p += 1
            if p < len(inp):
                computer.set_input(ord(inp[p]))

        if retcode == 1 and retval > 255:
            return retval

    return steps
    

def read_and_solve():
    with open('input_21.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())