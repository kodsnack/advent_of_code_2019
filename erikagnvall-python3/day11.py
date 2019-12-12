import os.path
from queue import Queue
from threading import Thread

from intcode import run


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        return [int(l) for l in f.readline().split(',')]


def _run_robot(program, inputs, outputs):
    run(list(program), inputs=inputs, outputs=outputs)


def _paint(program, start=0):
    inputs = Queue()
    outputs = Queue()
    t = Thread(target=_run_robot, args=(program, inputs, outputs))
    t.start()
    pos = (0, 0)
    whites = set()
    if start == 1:
        whites.add(pos)
    visited = set()
    direction = 0
    while t.is_alive():
        if pos in whites:
            inputs.put(1)
        else:
            inputs.put(0)

        color = outputs.get()
        if color == 1:
            whites.add(pos)
        else:
            whites.discard(pos)
        visited.add(pos)

        turn = outputs.get()
        if turn == 0:
            direction = (direction - 1) % 4
        elif turn == 1:
            direction = (direction + 1) % 4

        if direction == 0:  # up
            pos = (pos[0], pos[1] - 1)
        elif direction == 1:  # right
            pos = (pos[0] + 1, pos[1])
        elif direction == 2:  # down
            pos = (pos[0], pos[1] + 1)
        elif direction == 3:  # left
            pos = (pos[0] - 1, pos[1])
    return whites, len(visited)


def part1(program):
    _, n = _paint(program)
    return n


def part2(program):
    whites, _ = _paint(program, start=1)
    min_x = min(whites, key=lambda x: x[0])[0]
    min_y = min(whites, key=lambda x: x[1])[1]
    max_x = max(whites, key=lambda x: x[0])[0]
    max_y = max(whites, key=lambda x: x[1])[1]
    code = [[0] * (max_x + 2 - min_x) for _ in range(min_y, max_y + 1)]
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x+1):
            code[y][x] = 1 if (x, y) in whites else 0
            print('#' if code[y][x] == 1 else ' ', end='')
        print()
    return code


program = _read_input()
if __name__ == '__main__':
    print(part1(program))
    part2(program)


############
# Tests


def test_solutions():
    assert part1(program) == 2478

    part2_answer = [
        [0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0,
         0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1],
        [0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1,
         0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1],
        [0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1,
         0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0],
        [0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0,
         0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0],
        [0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0,
         0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0],
        [0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1,
         0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1],
    ]
    assert part2(program) == part2_answer
