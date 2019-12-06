from utils import read_input, write_output, check_result


def calc(lines):
    result = 0
    for word in lines.split():
        fuel = int(int(word) / 3 - 2)
        while fuel > 0:
            result += fuel
            fuel = int(fuel / 3 - 2)
    return result


if __name__ == '__main__':
    lines = read_input()
    result = str(calc(lines))
    write_output(result)
    check_result(result)
