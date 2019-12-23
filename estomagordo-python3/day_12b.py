import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def get_state(moons, coord):
    statelist = []

    for moon in moons:
        statelist.append(moon[coord])
        statelist.append(moon[coord + 3])

    return tuple(statelist)


def solve(moons):
    for i in range(len(moons)):
        moons[i] = [0, 0, 0] + moons[i]

    steps = 0

    initial_xstate = get_state(moons, 0)
    initial_ystate = get_state(moons, 1)
    initial_zstate = get_state(moons, 2)
    
    repeats = [-1, -1, -1]
    
    while True:
        velchanges = [[0, 0, 0] for _ in range(len(moons))]
        
        for i in range(len(moons) - 1):
            ix, iy, iz = moons[i][3:]
            for j in range(i + 1, len(moons)):
                jx, jy, jz = moons[j][3:]

                if ix > jx:
                    velchanges[i][0] -= 1
                    velchanges[j][0] += 1
                elif ix < jx:
                    velchanges[i][0] += 1
                    velchanges[j][0] -= 1

                if iy > jy:
                    velchanges[i][1] -= 1
                    velchanges[j][1] += 1
                elif iy < jy:
                    velchanges[i][1] += 1
                    velchanges[j][1] -= 1

                if iz > jz:
                    velchanges[i][2] -= 1
                    velchanges[j][2] += 1
                elif iz < jz:
                    velchanges[i][2] += 1
                    velchanges[j][2] -= 1

        for i, deltas in enumerate(velchanges):
            dx, dy, dz = deltas
            moons[i][0] += dx
            moons[i][1] += dy
            moons[i][2] += dz

        for i, moon in enumerate(moons):
            moons[i][3] += moon[0]
            moons[i][4] += moon[1]
            moons[i][5] += moon[2]

        steps += 1

        xstate = get_state(moons, 0)
        ystate = get_state(moons, 1)
        zstate = get_state(moons, 2)
        
        if xstate == initial_xstate and repeats[0] == -1:
            repeats[0] = steps
        if ystate == initial_ystate and repeats[1] == -1:
            repeats[1] = steps
        if zstate == initial_zstate and repeats[2] == -1:
            repeats[2] = steps

        if all(repeat > -1 for repeat in repeats):
            return helpers.lcm_many(repeats)


def read_and_solve():    
    with open('input_12.txt') as f:
        data = []
        for line in f:
            data.append(helpers.ints_from_line(line))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())