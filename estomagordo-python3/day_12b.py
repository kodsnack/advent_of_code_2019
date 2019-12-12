import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def solve(moons):
    # seen = set()
    # seen_a_pos = set()
    # seen_a_vel = set()
    # seen_b_pos = set()
    # seen_b_vel = set()
    # seen_c_pos = set()
    # seen_c_vel = set()
    # seen_d_pos = set()
    # seen_d_vel = set()

    for i in range(len(moons)):
        moons[i] = [0, 0, 0] + moons[i]

    initial = []

    for m in moons:
        initial.append(list(m))

    steps = 0

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

        if all(v == 0 for v in moons[0][:3]) and all(v == 0 for v in moons[1][:3]) and all(v == 0 for v in moons[2][:3]) and all(v == 0 for v in moons[3][:3]):
            print(steps)

        # if tuple(moons[3][:3]) in seen_a_vel:
        #     print(steps, 'a vel')
        # seen_a_vel.add(tuple(moons[3][:3]))

        # if tuple(moons[3][3:]) in seen_a_pos:
        #     print(steps, 'a pos')
        # seen_a_pos.add(tuple(moons[3][3:]))

        # statelist = []

        # for moon in moons:
        #     statelist += moon

        # state = tuple(statelist)
        
        # if state in seen:
        #     return steps
        # seen.add(state)

        if moons == initial:
            return steps

        steps += 1
        if steps % 1000000 == 0:
            print(steps)

def read_and_solve():
    pattern = re.compile(r'-?\d+')
    with open('input_12.txt') as f:
        data = []
        for line in f:
            data.append([int(val) for val in re.findall(pattern, line) if val])
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())