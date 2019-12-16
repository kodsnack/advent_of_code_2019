import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from math import ceil


def set_recipe(elements, original, name, amount):
    elements[name][0] = amount

    for req_i in range(len(elements[name][3])):
        elements[name][3][req_i][1] = original[req_i] * amount


def solve(data):
    trillion = 1000000000000

    original_fuel = []

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

            requirements.append([name, cost])

        elements[last] = [last_amount, 0, 0, requirements]

        if last == 'FUEL':
            for req in requirements:
                original_fuel.append(req[1])

    fuel_need = 1
    elements['ORE'] = [1, 0, 0, []]

    while elements['ORE'][2] < trillion and fuel_need > 0:
        set_recipe(elements, original_fuel, 'FUEL', fuel_need)
        rem_ore = elements['ORE'][2]
        elements['FUEL'][1] += fuel_need

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
                break

        used_ore = elements['ORE'][2] - rem_ore
        ore_per_fuel = used_ore // fuel_need
        fuel_need = (trillion - elements['ORE'][2]) // ore_per_fuel
        
        if fuel_need > 10:
            fuel_need -= 8 # Make sure we don't overshoot.

    return elements['FUEL'][2]
    

def read_and_solve():
    with open('input_14.txt') as f:
        data = [line.split() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())