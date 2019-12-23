import re

from heapq import heappop, heappush
from collections import Counter, defaultdict, deque
from intcode import Computer


def solve(d):
    size = 50
    computers = [Computer(d, i) for i in range(size)]
    outbound = [[] for i in range(size)]
    inbound = [deque() for i in range(size)]

    while True:
        for i in range(50):
            retcode, retval = computers[i].step()
            
            if retcode == 1:
                outbound[i].append(retval)
                if len(outbound[i]) == 3:
                    if outbound[i][0] == 255:
                        return retval

                    destination, x, y = outbound[i]

                    if len(inbound[destination]) == 0 and computers[destination].input == -1:
                        computers[destination].set_input(x)
                    else:
                        inbound[destination].append(x)
                        
                    inbound[destination].append(y)

                    outbound[i] = []
            elif retcode == 0 and retval == 3:
                if len(inbound[i]) > 0:
                    computers[i].set_input(inbound[i].popleft())
                else:
                    computers[i].set_input(-1)



    

def read_and_solve():
    with open('input_23.txt') as f:
        data = list(map(int, f.readline().split(',')))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())