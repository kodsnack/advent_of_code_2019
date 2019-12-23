import re

from heapq import heappop, heappush
from collections import Counter, defaultdict, deque
from intcode import Computer


def solve(d):
    size = 50
    computers = [Computer(d, i) for i in range(size)]
    outbound = [[] for i in range(size)]
    inbound = [deque() for i in range(size)]
    nat = []
    prevnat = []
    first = True
    steps = 0

    while True:
        for i in range(50):
            retcode, retval = computers[i].step()
            
            if retcode == 1:
                outbound[i].append(retval)
                if len(outbound[i]) == 3:
                    destination, x, y = outbound[i]

                    if destination == 255:                  
                        nat = [x, y]                     
                    else:
                        if len(inbound[destination]) == 0 and computers[destination].input == -1:
                            computers[destination].set_input(x)
                        else:
                            inbound[destination].append(x)
                            
                        inbound[destination].append(y)

                    outbound[i] = []
            elif retcode == 0 and retval == 3:
                if len(inbound[i]) > 0:
                    computers[i].set_input(inbound[i].popleft())
                    first = False
                else:
                    computers[i].set_input(-1)

        if not first and all(len(inb) == 0 for inb in inbound) and all(c.input == -1 for c in computers) and all(len(o) == 0 for o in outbound):
            if prevnat and prevnat[1] == nat[1]:
                return nat[1]
            if computers[0].input == -1:
                computers[0].set_input(nat[0])
            else:
                inbound[0].append(nat[0])
            inbound[0].append(nat[1])
        
        steps += 1



    

def read_and_solve():
    with open('input_23.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())