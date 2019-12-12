import os.path

from intcode import run


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        return tuple(int(i) for i in f.readline().split(','))


def part1(program):
    _, _, _, output = run(list(program), inputs=[1])
    return output


def part2(program):
    _, _, _, output = run(list(program), inputs=[2])
    return output


program = _read_input()
if __name__ == '__main__':
    print(part1(program))
    print(part2(program))


############
# Tests


def test_examples():
    example = [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
    output = part1(example)
    assert example == output

    example = [1102, 34915192, 34915192, 7, 4, 7, 99, 0]
    output = part1(example)
    assert len(str(output[0])) == 16

    example = [104, 1125899906842624, 99]
    output = part1(example)
    assert output[0] == 1125899906842624


def test_solutions():
    assert part1(program) == [2789104029]
    assert part2(program) == [32869]
