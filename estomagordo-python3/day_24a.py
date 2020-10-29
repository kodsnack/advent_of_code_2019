import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def biodiversity(board):
    score = 0

    for y in range(5):
        for x in range(5):
            if board[y][x] != '#':
                continue
            score += 2**(y*5 + x)

    return score


def progress(board):
    new_board = []

    for y in range(5):
        new_row = []
        for x in range(5):
            bug = board[y][x] == '#'
            neighbours = helpers.get_moves(5, 5, y, x)
            neighbour_bug_count = 0
            
            for ny, nx in neighbours:
                if board[ny][nx] == '#':
                    neighbour_bug_count += 1

            if (bug and neighbour_bug_count == 1) or (not bug and neighbour_bug_count in (1, 2)):
                new_row.append('#')
            else:
                new_row.append('.')
        new_board.append(new_row)

    return new_board


def solve(board):
    seen = { biodiversity(board) }

    while True:
        board = progress(board)
        bio = biodiversity(board)
        
        if bio in seen:
            return bio

        seen.add(bio)
    

def read_and_solve():
    with open('input_24.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())