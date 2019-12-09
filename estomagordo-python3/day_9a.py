import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from itertools import permutations


def solve(data, inp):
    #ata = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    d = defaultdict(int)

    for i, v in enumerate(data):
        d[i] = v

    p = 0
    relbase = 0
    steps = 0

    while True:
        steps += 1
        amode = (d[p] % 1000) // 100
        bmode = (d[p] % 10000) // 1000
        a = d[p + 1] if amode == 1 else d[d[p + 1]] if amode == 0 else d[d[p + 1] + relbase]
        b = d[p + 2] if bmode == 1 else d[d[p + 2]] if bmode == 0 else d[d[p + 2] + relbase]

        if d[p] % 100 == 99:
            return 'ninety-nine', d[0], steps
        elif d[p] % 100 == 1:
            d[d[p + 3]] = a + b
            p += 4
        elif d[p] % 100 == 2:
            d[d[p + 3]] = a * b
            p += 4
        elif d[p] % 100 == 3:
            d[a] = inp
            p += 2
        elif d[p] % 100 == 4:
            print('output', a, d[a], steps)
            p += 2
        elif d[p] % 100 == 5:
            if a != 0:
                p = b
            else:
                p += 3
        elif d[p] % 100 == 6:
            if a == 0:
                p = b
            else:
                p += 3
        elif d[p] % 100 == 7:
            c = 1 if a < b else 0
            d[d[p + 3]] = c
            p += 4
        elif d[p] % 100 == 8:
            c = 1 if a == b else 0
            d[d[p + 3]] = c
            p += 4
        elif d[p] % 100 == 9:
            relbase += a
            p += 2
        else:
            print('uh oh', d[p])

    return 'outside-loop', d[0], steps


def read_and_solve():
    with open('input_9.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data, 1)

if __name__ == '__main__':
    print(read_and_solve())