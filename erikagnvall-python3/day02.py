import itertools
import os.path

from intcode import run


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        return [int(l) for l in f.readline().split(',')]


def part1(program):
    memory = program.copy()
    memory[1] = 12
    memory[2] = 2
    memory, _, _, _ = run(memory)
    return memory[0]


def part2(program: list):
    inputs = itertools.product(range(100), range(100))
    for n, v in inputs:
        memory = program.copy()
        memory[1] = n
        memory[2] = v
        memory, _, _, _ = run(memory)
        if memory[0] == 19690720:
            break
    return n * 100 + v


program = _read_input()
print(part1(program))
print(part2(program))


############
# Tests


def test_run():
    assert run([1, 0, 0, 0, 99])[0] == [2, 0, 0, 0, 99]
    assert run([2, 3, 0, 3, 99])[0] == [2, 3, 0, 6, 99]
    assert run([2, 4, 4, 5, 99, 0])[0] == [2, 4, 4, 5, 99, 9801]
    assert run([1, 1, 1, 4, 99, 5, 6, 0, 99])[0] == [30, 1, 1, 4, 2, 5, 6, 0, 99]


def test_solution_part1():
    assert part1(program) == 5305097


def test_solution_part2():
    assert part2(program) == 4925
