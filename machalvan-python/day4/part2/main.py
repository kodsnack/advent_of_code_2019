from utils import read_input, write_output, check_result
import re
from collections import Counter


def has_six_digits(x):
    return len(x) == 6


def has_two_adj_digits_not_part_of_larger_group(x):
    return 2 in Counter(x).values()


def digits_never_decrease(x):
    pointer = 1
    while pointer < len(x):
        if x[pointer - 1] > x[pointer]:
            return False
        pointer += 1
    return True


def calc(lines):
    result = 0
    parser = re.compile("\d+")
    values = [int(x) for line in lines for x in parser.findall(line.strip())]

    for x in range(values[0], values[1] + 1):
        x = str(x)
        if has_six_digits(x) and has_two_adj_digits_not_part_of_larger_group(x) and digits_never_decrease(x):
            result += 1
    return result


if __name__ == '__main__':
    lines = read_input()
    result = str(calc(lines))
    write_output(result)
    check_result(result)
