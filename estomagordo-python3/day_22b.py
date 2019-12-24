import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


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


def run(instructions, size, position):
    front, back = position, size - position - 1
    reversed = False

    for instruction in instructions[::-1]:
        if instruction[-1] == 'stack':
            reversed = not reversed
        elif instruction[0] == 'deal':
            increment = int(instruction[-1])
            if reversed:
                increment *= -1
            front, back = deal_with_increment(front, size, increment), deal_with_increment(back, size, increment)
        else:
            amount = int(instruction[-1])
            if reversed:
                front, back = (back + amount) % size, (front + amount) % size
            else:
                front, back = (front + amount) % size, (back + amount) % size

    return back if reversed else front

def solve(instructions, size, times, position):
    return run(instructions, size, position)
    

def read_and_solve():
    with open('input_22.txt') as f:
        data = [line.split() for line in f]
        for x in range(10):
            print(run(data, 10, x), end=' ')
        # return solve(data, 10007, 1, 2019)
        # return solve(data, 119315717514047 , 101741582076661, 2020)

if __name__ == '__main__':
    print(read_and_solve())