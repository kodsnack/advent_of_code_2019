import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def solve(d):
    count = 0

    for x in range(50):
        for y in range(50):
            computer = Computer(d, x)

            while True:
                retcode, retval = computer.step()

                if retcode == 0 and retval == 3:
                    computer.set_input(y)
                    break

            retcode, retval = computer.get_output()

            if retval == 1:
                count += 1

    return count
    

def read_and_solve():
    with open('input_19.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())