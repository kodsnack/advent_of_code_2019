import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def apply(deck, instruction, size):
    if instruction[-1] == 'stack':
        return deck[::-1]

    if instruction[0] == 'cut':
        cut = int(instruction[1])
        return deck[cut:] + deck[:cut]

    increment = int(instruction[-1])

    val = 0
    new_deck = [0 for _ in range(size)]

    for x in range(size):
        new_deck[val] = deck[x]
        val = (val + increment) % size

    return new_deck


def solve(instructions, size):
    deck = [x for x in range(size)]

    for instruction in instructions:
        deck = apply(deck, instruction, size)
        
    for i, card in enumerate(deck):
        if card == 2019:
            return i
    return deck


def read_and_solve():
    with open('input_22.txt') as f:
        data = [line.split() for line in f]
        return solve(data, 10007)

if __name__ == '__main__':
    print(read_and_solve())