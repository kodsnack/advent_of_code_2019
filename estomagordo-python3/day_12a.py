import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(moons, steps):
    for i in range(len(moons)):
        moons[i] = [0, 0, 0] + moons[i]

    for _ in range(steps):
        velchanges = [[0, 0, 0] for _ in range(len(moons))]
        d = 22
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

    energy = 0

    for moon in moons:
        kineng = abs(moon[0]) + abs(moon[1]) + abs(moon[2])
        poteng = abs(moon[3]) + abs(moon[4]) + abs(moon[5])
        energy += kineng * poteng

    return energy
    

def read_and_solve():
    pattern = re.compile(r'-?\d+')
    with open('input_12.txt') as f:
        data = []
        for line in f:
            data.append([int(val) for val in re.findall(pattern, line) if val])
        return solve(data, 1000)

if __name__ == '__main__':
    print(read_and_solve())