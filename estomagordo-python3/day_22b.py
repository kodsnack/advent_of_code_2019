import re

from heapq import heappop, heappush
from functools import reduce


def run(instructions, size, position):
    for instruction in instructions:
        if instruction[-1] == 'stack':
            position = size - position - 1
        elif instruction[0] == 'deal':
            increment = int(instruction[-1])
            position = (position * increment) % size
        else:
            amount = int(instruction[-1])
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
    times = size - times - 1
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
    

def read_and_solve():
    with open('input_22.txt') as f:
        data = [line.split() for line in f]
        return solve(data, 119315717514047, 101741582076661, 2020)

if __name__ == '__main__':
    print(read_and_solve())