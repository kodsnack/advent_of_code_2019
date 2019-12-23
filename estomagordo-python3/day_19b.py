import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def solve(d):
    size = 1000
    hit = set()
    extremes = [[0, 0] for _ in range(size)]
    # seen_wide = False
    # wide_width = 200

    for y in range(size):
        for x in range(y, 2 * y):
            computer = Computer(d, x)

            while True:
                retcode, retval = computer.step()

                if retcode == 0 and retval == 3:
                    computer.set_input(y)
                    break

            retcode, retval = computer.get_output()

            if retval == 1:
                # if (y, x - wide_width) in hit:
                #     seen_wide = True
                hit.add((y, x))
                # if seen_wide and (y - 99) in hit:
                if (y - 99, x + 99) in hit:
                    return 10000 * x + y - 99
                if extremes[y][0] == 0:
                    extremes[y][0] = x
                    if (y - 99, x + 99) in hit:
                        print(y, x)
                extremes[y][1] = x

    return len(hit)
    

def read_and_solve():
    with open('input_19.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())