import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from itertools import permutations
from intcode import Computer


def solve(data, inp):
    painted = defaultdict(int)
    y = 0
    x = 0
    output_count = 0    
    directions = ((-1, 0), (0, 1), (1, 0), (0, -1))
    facing = 0
    computer = Computer(data, inp)
    retval, retcode = 0, 0

    while retcode != -1:
        retcode, retval = computer.step()

        if retcode == 0:
            computer.set_input(painted[(y, x)])
            continue

        if output_count % 2 == 0:
            painted[(y, x)] = retval
        else:
            if retval == 0:
                facing = (facing - 1) % 4
            else:
                facing = (facing + 1) % 4

            y += directions[facing][0]
            x += directions[facing][1]

        computer.set_input(painted[(y, x)])

        output_count += 1

    return len(painted)


def read_and_solve():
    with open('input_11.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data, 0)

if __name__ == '__main__':
    print(read_and_solve())