import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def solve(d):
    size = 1000
    hit = set()
    extremes = [[0, 0] for _ in range(size)]

    for y in range(size):
        # print(y)
        for x in range(y, 2 * y):
            computer = Computer(d, x)

            while True:
                retcode, retval = computer.step()

                if retcode == 0 and retval == 3:
                    computer.set_input(y)
                    break

            retcode, retval = computer.get_output()

            if retval == 1:
                hit.add((y, x))
                if extremes[y][0] == 0:
                    extremes[y][0] = x
                    if (y - 99, x + 99) in hit:
                        print(y, x)
                extremes[y][1] = x

    # for y in range(500):
    #     line = []
    #     for x in range(500):
    #         line.append('#' if (y, x) in hit else ' ')
    #     print(''.join(line))

    # for y in range(400, size):
    #     print(y, extremes[y][0], extremes[y][1])

    return len(hit)
    

def read_and_solve():
    with open('input_19.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())