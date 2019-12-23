import os.path

from intcode import run


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        program = tuple(int(i) for i in f.readline().split(','))
    return program


def part1(memory):
    _, _, _, outputs = run(memory, inputs=[1])
    return outputs[-1]


def part2(memory):
    _, _, _, outputs = run(memory, inputs=[5])
    return outputs[-1]


program = _read_input()
if __name__ == '__main__':
    print(part1(list(program)))
    print(part2(list(program)))


############
# Tests


def test_run_example():
    mem, ip, inputs, outputs = run([3, 0, 4, 0, 99], inputs=[42])
    assert mem == [42, 0, 4, 0, 99]
    assert ip == 4
    assert not inputs
    assert outputs == [42]

    memory = [1002, 4, 3, 4, 33]
    assert run(memory) == ([1002, 4, 3, 4, 99], 4, [], [])

    memory = [1101, 100, -1, 4, 0]
    assert run(memory) == ([1101, 100, -1, 4, 99], 4, [], [])


def test_opcode8():

    memory = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
    _, _, _, outputs = run(memory, inputs=[8])
    assert outputs == [1]

    memory = [3, 3, 1108, -1, 8, 3, 4, 3, 99]
    _, _, _, outputs = run(memory, inputs=[8])
    assert outputs == [1]

    memory = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
    _, _, _, outputs = run(memory, inputs=[4])
    assert outputs == [0]

    memory = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
    _, _, _, outputs = run(memory, inputs=[4])
    assert outputs == [0]


def test_opcode7():
    memory = [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]
    _, _, _, outputs = run(memory, inputs=[8])
    assert outputs == [0]

    memory = [3, 3, 1107, -1, 8, 3, 4, 3, 99]
    _, _, _, outputs = run(memory, inputs=[8])
    assert outputs == [0]

    memory = [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]
    _, _, _, outputs = run(memory, inputs=[4])
    assert outputs == [1]

    memory = [3, 3, 1107, -1, 8, 3, 4, 3, 99]
    _, _, _, outputs = run(memory, inputs=[4])
    assert outputs == [1]


def test_jumps():
    memory = [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]
    _, _, _, outputs = run(memory, inputs=[0])
    assert outputs == [0]

    memory = [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]
    _, _, _, outputs = run(memory, inputs=[0])
    assert outputs == [0]

    memory = [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]
    _, _, _, outputs = run(memory, inputs=[123])
    assert outputs == [1]

    memory = [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]
    _, _, _, outputs = run(memory, inputs=[123])
    assert outputs == [1]


def test_part1():
    assert part1(list(program)) == 14155342


def test_part2():
    assert part2(list(program)) == 8684145
