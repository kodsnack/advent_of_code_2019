import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


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
            position = shuffler(increment, position, size)
            # deal_with_increment(position, size, increment)#position = increment * orig
        else:
            amount = int(instruction[-1])
            position = (position + amount) % size

    return position


def solve(instructions, size, times, position):
    from collections import Counter
    c = Counter()
    for _ in range(times):
        position = run(instructions, size, position)
        c[position] += 1
    return [val for val in c if c[val] > 1]
    

def read_and_solve():
    with open('input_22.txt') as f:
        data = [line.split() for line in f]
        # print(solve(data, 10, 1, 3))
        # print(solve(data, 10007, 1, 2019))
        # print(solve(data, 10007, 1, 6326))
        # from random import shuffle
        # l = list(data)
        # shuffle(l)
        # print(solve(l, 119315717514047, 1, 2020))
        # print(119315717514047)
        return solve(data, 119315717514047, 30000, 2020)
        # return solve(data, 119315717514047, 101741582076661, 2020)

if __name__ == '__main__':
    print(read_and_solve())

# print(modsolver(3, 6, 10))