import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from intcode import Computer


def get_data():
    items = [
        (1, 0, 'mutex'),
        (2, 0, 'manifold'),
        (2, -2, 'klein bottle'),
        (1, 1, 'mug'),
        (1, 2, 'polygon'),
        (0, 2, 'loom'),
        (0, 4, 'photons'),
        (1, 5, 'pointer')
        ]

    paths = {
        (0, 0):  [(-1, 0), (0, -1), (1, 0)],
        (-1, 0): [(0, 0)],
        (0, -1): [(0, 0)],
        (1, 0):  [(0, 0), (1, 1), (2, 0)],
        (2, 0):  [(1, 0), (2, -1), (2, 1)],
        (2, 1):  [(2, 0)],
        (2, -1): [(2, 0), (3, -1), (2, -2)],
        (1, 1):  [(1, 0), (1, 2)],
        (1, 2):  [(1, 1), (0, 2), (1, 3)],
        (0, 2):  [(1, 2), (-1, 2)],
        (-1, 2): [(0, 2)],
        (1, 3):  [(1, 2), (1, 4)],
        (1, 4):  [(1, 3), (0, 4), (1, 5)],
        (1, 5):  [(1, 4), (2, 5)],
        (2, 5):  [(1, 5), (2, 4)]
    }

    return items, paths


def get_program():
    return """south
take mutex
south
take manifold
west
west
take klein bottle
east
east
north
east
take mug
east
take polygon
north
take loom
south
east
east
east
take pointer
south
"""


def solve(d):
    items, paths = get_data()
    program = get_program()
    computer = Computer(d, ord(program[0]))
    p = 1

    while True:
        retcode, retval = computer.step()
        
        if retcode == 1:
            print(chr(retval), end='')
            if (chr(retval)) == '?':
                print('')
                inp = input() + '\n'
                if not program:
                    program = inp
                    computer.set_input(ord(program[0]))
                    p = 1
        elif retcode == 0 and retval == 3:
            if p < len(program):
                computer.set_input(ord(program[p]))
                p += 1
            else:
                program = ''
        elif retcode == -1:
            return
    

def read_and_solve():
    with open('input_25.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())