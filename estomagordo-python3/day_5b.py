import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def solve(d, inp):
    computer = Computer(d, inp)
    answer = 0
    
    while True:
        retcode, retval = computer.get_output()

        if retcode == 1:
            answer = retval
        else:
            return answer
            

def read_and_solve():
    with open('input_5.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data, 5)

if __name__ == '__main__':
    print(read_and_solve())