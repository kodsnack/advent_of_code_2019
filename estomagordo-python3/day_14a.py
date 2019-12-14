import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from math import ceil


def solve(data):
    reactions = {}

    for line in data:
        last = line[-1]
        last_amount = int(line[-2])
        requirements = []
        l = len(line)

        for x in range(0, l - 3, 2):
            cost = int(line[x])
            name = line[x + 1]

            if name[-1] == ',':
                name = name[:-1]

            requirements.append([name, cost])

        reactions[last] = [last_amount, requirements]

    needed = defaultdict(int)

    investigating = reactions['FUEL'][1]

    for name, amount_needed in investigating:
        needed[name] = amount_needed

    reacted = True
    while reacted:
        reacted = False
        removing = []
        adding = defaultdict(int)

        for name, amount_needed in needed.items():
            produced, reaction = reactions[name]

            if len(reaction) == 1 and reaction[0][0] == 'ORE':
                continue

            times = ceil(amount_needed / produced)

            for elem, cost in reaction:
                adding[elem] += cost * times
                # adding.append([elem, cost * times])

            reacted = True
            removing.append(name)

        for r in removing:
            del needed[r]

        bo = 2
        
        # needed = adding

        for elem, amount in adding.items():
            needed[elem] += amount

    ore = 0

    for elem, amount_needed in needed.items():
        produced, reaction = reactions[elem]
        times = ceil(amount_needed / produced)
        ore += times * reaction[0][1]

    return ore
    

def read_and_solve():
    with open('input_14.txt') as f:
        data = [line.split() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())