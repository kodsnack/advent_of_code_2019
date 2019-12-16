import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def solve_part(d, noun, verb):
    d[1] = noun
    d[2] = verb

    computer = Computer(d, 0)
    
    while True:
        retcode, retval = computer.get_output()

        if retcode == -1:
            return retval


def solve(d):
    for noun in range(100):
        for verb in range(100):
            if solve_part(list(d), noun, verb) == 19690720:
                return 100 * noun + verb
    

def read_and_solve():
    with open('input_2.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())