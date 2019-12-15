import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from math import ceil


def heuristic(elements, fuel_need):
    tot = 0

    for x in range(len(elements)):
        tot += max(0, fuel_need[x] - elements[x])

    return tot


def solve(data):
    reactions = {}
    name_to_pos = {}
    pos_to_name = {}
    names = set()

    for i, line in enumerate(data):
        last = line[-1]
        last_amount = int(line[-2])
        requirements = []
        l = len(line)

        names.add(last)

        for x in range(0, l - 3, 2):
            cost = int(line[x])
            name = line[x + 1]

            if name[-1] == ',':
                name = name[:-1]

            names.add(name)

            requirements.append((name, cost))

        reactions[(i, tuple(requirements))] = [last_amount, last]

    for i, name in enumerate(sorted(names)):
        name_to_pos[name] = i
        pos_to_name[i] = name

    fuel_need = [0 for _ in range(len(names))]

    for req, result in reactions.items():
        res_amount, res_name = result
        
        if res_name != 'FUEL':
            continue

        i, requirements = req

        for req_name, req_cost in requirements:
            fuel_need[name_to_pos[req_name]] = req_cost
    
    frontier = [[heuristic([0 for _ in range(len(names))], fuel_need), 0, tuple([0 for _ in range(len(names))])]]
    seen = set()
    
    while True:
        remaining, used, elements = heappop(frontier)
        print(remaining, used)

        if remaining == 0:
            return used

        for req, result in reactions.items():
            _, requirement = req
            result_amount, result_name = result

            if len(requirement) == 1 and requirement[0][0] == 'ORE':
                # print('ore')
                new_els = list(elements)
                new_els[name_to_pos[result_name]] += result_amount
                if tuple(new_els) not in seen:
                    score = heuristic(new_els, fuel_need)
                    heappush(frontier, (score, used + requirement[0][1], tuple(new_els)))
                    seen.add(tuple(new_els))
                continue
            # print('not ore')
            possible = True
            # print(requirement, elements)
            for name, needed in requirement:
                if elements[name_to_pos[name]] < needed:
                    possible = False
                    break

            if not possible:
                continue
            
            new_els = list(elements)

            for name, needed in requirement:
                new_els[name_to_pos[name]] -=  needed

            new_els[name_to_pos[result_name]] += result_amount
            if tuple(new_els) not in seen:
                score = heuristic(new_els, fuel_need)
                heappush(frontier, (score, used, tuple(new_els)))
                seen.add(tuple(new_els))
    

def read_and_solve():
    with open('input_14.txt') as f:
        data = [line.split() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())