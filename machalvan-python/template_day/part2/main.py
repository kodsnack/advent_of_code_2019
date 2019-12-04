from utils import read_input, write_output, check_result
import re


def calc(lines):
    result = ''
    parser = re.compile("-?\d+")
    values = [int(x) for line in lines for x in parser.findall(line.strip())]

    for value in values:
        print(value)  # Code here
    return result


if __name__ == '__main__':
    lines = read_input()
    result = str(calc(lines))
    write_output(result)
    check_result(result)
