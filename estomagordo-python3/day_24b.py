import helpers
import re

from heapq import heappop, heappush
from collections import Counter, defaultdict


def progress(system):
    new_system = {}
    lowest = 0
    highest = 0

    for level, board in system.items():
        lowest = min(lowest, level)
        highest = max(highest, level)
        
        new_board = []

        for y in range(5):
            new_row = []
            for x in range(5):
                if y == 2 and x == 2:
                    new_row.append('.')
                    continue

                bug = board[y][x] == '#'
                neighbours = helpers.get_moves(5, 5, y, x)
                neighbour_bug_count = 0
                
                for ny, nx in neighbours:
                    if board[ny][nx] == '#':
                        neighbour_bug_count += 1

                if level - 1 in system:
                    if y == 0 and system[level - 1][1][2] == '#':
                        neighbour_bug_count += 1
                    if x == 0 and system[level - 1][2][1] == '#':
                        neighbour_bug_count += 1
                    if y == 4 and system[level - 1][3][2] == '#':
                        neighbour_bug_count += 1
                    if x == 4 and system[level - 1][2][3] == '#':
                        neighbour_bug_count += 1

                if level + 1 in system:
                    if y == 1 and x == 2:
                        neighbour_bug_count += system[level + 1][0].count('#')
                    if y == 2 and x == 1:
                        for ny in range(5):
                            if system[level + 1][ny][0] == '#':
                                neighbour_bug_count += 1
                    if y == 2 and x == 3:
                        for ny in range(5):
                            if system[level + 1][ny][4] == '#':
                                neighbour_bug_count += 1
                    if y == 3 and x == 2:
                        neighbour_bug_count += system[level + 1][4].count('#')

                if (bug and neighbour_bug_count == 1) or (not bug and neighbour_bug_count in (1, 2)):
                    new_row.append('#')
                else:
                    new_row.append('.')
            new_board.append(new_row)

        new_system[level] = new_board

    new_low_board = [['.'] * 5 for _ in range(5)]

    if system[lowest][0].count('#') in (1, 2):
        new_low_board[1][2] = '#'
    if system[lowest][4].count('#') in (1, 2):
        new_low_board[3][2] = '#'
    left_count = 0
    for ly in range(5):
        if system[lowest][ly][0] == '#':
            left_count += 1
    if left_count in (1, 2):
        new_low_board[2][1] = '#'
    right_count = 0
    for ly in range(5):
        if system[lowest][ly][4] == '#':
            right_count += 1
    if right_count in (1, 2):
        new_low_board[2][3] = '#'

    if any(row.count('#') > 0 for row in new_low_board):
        new_system[lowest - 1] = new_low_board

    new_high_board = [['.'] * 5 for _ in range(5)]

    if system[highest][1][2] == '#':
        for nx in range(5):
            new_high_board[0][nx] = '#'
    if system[highest][3][2] == '#':
        for nx in range(5):
            new_high_board[4][nx] = '#'
    if system[highest][2][1] == '#':
        for ny in range(5):
            new_high_board[ny][0] = '#'
    if system[highest][2][3] == '#':
        for ny in range(5):
            new_high_board[ny][4] = '#'

    if any(row.count('#') > 0 for row in new_high_board):
        new_system[highest + 1] = new_high_board

    return new_system


def solve(board, time_limit):
    levels = { 0: board }

    for _ in range(time_limit):
        levels = progress(levels)

    count = 0

    for level in levels.values():
        for row in level:
            count += row.count('#')

    return count
    

def read_and_solve():
    with open('input_24.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data, 200)

if __name__ == '__main__':
    print(read_and_solve())