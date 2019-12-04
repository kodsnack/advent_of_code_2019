import os.path


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        lines = f.readlines()
        # TODO
    return lines


def part1():
    # TODO
    pass


def part2():
    # TODO
    pass


input_ = _read_input()
print(part1(input_))
print(part2(input_))


############
# Tests
