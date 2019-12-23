import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from itertools import permutations
from intcode import Computer

def solve(d, lo, hi, ampcount):
    best = 0

    for perm in permutations(range(lo, hi)):
        computers = [Computer(d, perm[pos]) for pos in range(ampcount)]
        inp = 0
        step = 0

        while True:
            computer = computers[step % ampcount]
            
            if step < ampcount:
                computer.set_input(perm[step])
            else:
                computer.set_input(inp)

            retcode, retval = 0, 0

            while retcode == 0:
                retcode, retval = computer.step()

                if step < ampcount and retcode == 0 and retval == 3:
                    computer.set_input(inp)

                if retcode == 1:
                    inp = retval

            if retcode == -1:
                break

            step += 1

        best = max(best, inp)
        

    return best


def read_and_solve():
    with open('input_7.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data, 0, 5, 5)

if __name__ == '__main__':
    print(read_and_solve())