import re

from heapq import heappop, heappush
from collections import Counter, defaultdict
from functools import reduce


def extended_euclid(a, b):
    if a == 0 :   
        return b, 0, 1
             
    gcd, x1, y1 = extended_euclid(b % a, a) 
    
    x = y1 - (b // a) * x1  
    y = x1  
     
    return gcd, x, y


def modsolver(a, b, n):
    _, xp, _ = extended_euclid(a, b)
    return (xp * b) % n


def deal_with_increment(num, size, increment):
    used = 0
    pos = 0

    while True:
        fits = (size - pos - 1) // increment
        diff = num - pos
        
        if diff >= 0 and diff % increment == 0:
            return used + diff // increment

        pos = (pos + (fits + 1) * increment) % size
        used += fits + 1


def shuffler(increment, position, size):
    overshoot = 0

    while (size * overshoot + position) % increment:
        overshoot += 1

    return (size * overshoot + position) // increment


def run(instructions, size, position):
    for instruction in instructions[::-1]:
        if instruction[-1] == 'stack':
            position = size - position - 1
        elif instruction[0] == 'deal':
            increment = int(instruction[-1])
            # position = modsolver(increment, position, size)
            # position = shuffler(increment, position, size)
            # deal_with_increment(position, size, increment)
            position = (position * increment) % size
        else:
            amount = int(instruction[-1])
            # position = (position + amount) % size
            position = (position - amount) % size

    return position


def reduce_instructions(instructions, size):
    from heapq import heappop, heappush

    while True:
        into_index = -1

        for x, instruction in enumerate(instructions):
            if instruction[1] == 'into':
                into_index = x
                break

        if into_index == -1:
            break

        insert = [['deal', 'with', 'increment', str(size - 1)], ['cut', '1']]

        instructions = instructions[:x] + insert + instructions[x+1:]       

    t = tuple([tuple(line) for line in instructions])
    seen = {t}
    frontier = [(len(instructions), t)]

    while frontier:
        n, state = heappop(frontier)

        if n < 3:
            return state

        for x in range(1, n):
            if state[x][0] == 'cut':
                if state[x-1][0] == 'cut':
                    l = [list(line) for line in state]
                    l[x][-1] = str((int(l[x-1][-1]) + int(l[x][-1])) % size)
                    newstate = tuple([tuple(line) for line in l[:x-1] + l[x:]])
                    if newstate not in seen:
                        seen.add(newstate)
                        heappush(frontier, (len(newstate), newstate))
            elif state[x][1] == 'with':
                if state[x-1][1] == 'with':
                    l = [list(line) for line in state]
                    l[x][-1] = str((int(l[x][-1]) * int(l[x-1][-1])) % size)
                    newstate = tuple([tuple(line) for line in l[:x-1] + l[x:]])
                    if newstate not in seen:
                        seen.add(newstate)
                        heappush(frontier, (len(newstate), newstate))
                elif state[x-1][0] == 'cut':
                    l = [list(line) for line in state]
                    l[x-1], l[x] = l[x], l[x-1]
                    l[x][-1] = str((int(l[x][-1]) * int(l[x-1][-1])) % size)
                    newstate = tuple([tuple(line) for line in l])
                    if newstate not in seen:
                        seen.add(newstate)
                        heappush(frontier, (len(newstate), newstate))

    return len(min(seen))


def solve(instructions, size, times, position):
    times = size - times
    timesbin = str(bin(times))[2:]
    instructions = reduce_instructions(instructions, size)
    powers = [instructions]
    inscount = len(timesbin)

    for _ in range(inscount-1):
        instructions = reduce_instructions(instructions + instructions, size)
        powers.append(instructions)

    all_instructions = reduce(lambda a,b: a+b, [powers[x] for x in range(len(powers)) if timesbin[inscount-x-1] == '1'])

    master_instructions = reduce_instructions(all_instructions, size)
    
    return run(master_instructions, size, position)
    
    return timesbin
    return run(instructions, size, position)
    

def read_and_solve():
    with open('input_22.txt') as f:
        data = [line.split() for line in f]
        # return reduce_instructions(data, 10007)
        # print(solve(data, 10007, 7, 1000))
        # print(solve(data, 10007, 8, 1000))
        # print(solve(data, 10007, 9, 1000))
        return solve(data, 119315717514047, 101741582076661, 2020)

if __name__ == '__main__':
    print(read_and_solve())

# print(modsolver(3, 6, 10))

# 102627315445902
# 107988972293017
# 78009577690975
# 73832061268481