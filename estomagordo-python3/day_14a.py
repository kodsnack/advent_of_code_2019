import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from math import ceil


def solve(data):
    elements = {}

    for i, line in enumerate(data):
        last = line[-1]
        last_amount = int(line[-2])
        requirements = []
        l = len(line)

        for x in range(0, l - 3, 2):
            cost = int(line[x])
            name = line[x + 1]

            if name[-1] == ',':
                name = name[:-1]

            requirements.append((name, cost))

        elements[last] = [last_amount, 0, 0, requirements]

    elements['FUEL'][1] = 1
    elements['ORE'] = [1, 0, 0, []]

    while True:
        updated = False

        for element, info in elements.items():
            per_batch, needed, created, requirements = info
            
            if needed <= created:
                continue

            updated = True
            new_batches = ceil((needed - created) / per_batch)
            elements[element][2] += new_batches * per_batch

            for req_name, req_cost in requirements:
                elements[req_name][1] += req_cost * new_batches

            break

        if not updated:
            return elements['ORE'][2]
    

def read_and_solve():
    with open('input_14.txt') as f:
        data = [line.split() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())