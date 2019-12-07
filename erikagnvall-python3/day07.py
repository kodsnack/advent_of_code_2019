import os.path
from itertools import permutations
from queue import Queue
from threading import Thread

from intcode import run


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        return tuple(int(i) for i in f.readline().split(','))


def _run_amp(program, inputs, outputs):
    run(list(program), inputs=inputs, outputs=outputs)


def _run_amps(program, seq, start=0):
    # Set up input/output queues
    qs = [Queue() for _ in range(5)]
    for i, s in enumerate(seq):
        qs[i].put(s)
    qs[0].put(0)

    threads = []
    for i in range(5):
        threads.append(
            Thread(target=_run_amp, args=(program, qs[i], qs[(i + 1) % len(qs)]))
        )
        threads[-1].start()

    for t in threads:
        t.join()
    return qs[0].get()


def part1(program, phases=range(5)):
    seqs = permutations(phases, 5)
    max_signal = -1
    for s in seqs:
        signal = _run_amps(program, s)
        if signal > max_signal:
            max_signal = signal
    return max_signal


def part2(program):
    return part1(program, phases=range(5, 10))


program = _read_input()
print(part1(program))
print(part2(program))


############
# Tests

# fmt: off


def test_run_amps0():
    p = [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]
    seq = [4, 3, 2, 1, 0]
    assert _run_amps(p, seq) == 43210


def test_run_amps1():
    p = [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
         101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0]
    seq = [0, 1, 2, 3, 4]
    assert _run_amps(p, seq) == 54321


def test_run_amps2():
    p = [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
         1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0]
    seq = [1, 0, 4, 3, 2]
    assert _run_amps(p, seq) == 65210


def test_run_amps3():
    p = [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
         27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]
    seq = [9, 8, 7, 6, 5]
    assert _run_amps(p, seq) == 139629729


def test_part1_examples1():
    p = [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]
    assert part1(p) == 43210


def test_part1_examples2():
    p = [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
         101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0]
    assert part1(p) == 54321


def test_part1_examples3():
    p = [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
         1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0]
    assert part1(p) == 65210


def test_solutions():
    assert part1(program) == 101490
    assert part2(program) == 61019896
