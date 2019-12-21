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
    
    inp = """NOT A T
NOT B J
OR J T
NOT C J
OR J T
OR T J
AND D J
WALK
"""
    p = 0
    out = []
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

        if retcode == 1:
            print(chr(retval), end='')
            # out.append(retval)

    return steps
    # return ''.join(map(chr, out))
    

def read_and_solve():
    with open('input_21.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())