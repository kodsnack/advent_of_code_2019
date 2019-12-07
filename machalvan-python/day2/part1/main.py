from utils import read_input, write_output, check_result


def calc(lines):
    counter = 0
    words = lines.split(',')

    words[1] = 12
    words[2] = 2

    while counter < len(words):
        opcode = int(words[counter])
        if opcode == 1:
            counter += 1
            index = int(words[counter])
            value1 = int(words[index])

            counter += 1
            index = int(words[counter])
            value2 = int(words[index])

            sum = value1 + value2
            counter += 1
            index = int(words[counter])
            words[index] = sum

        elif opcode == 2:
            counter += 1
            index = int(words[counter])
            value1 = int(words[index])

            counter += 1
            index = int(words[counter])
            value2 = int(words[index])

            sum = value1 * value2
            counter += 1
            index = int(words[counter])
            words[index] = sum

        elif opcode == 99:
            break

        counter += 1

    result = words[0]
    return result


if __name__ == '__main__':
    lines = read_input()
    result = str(calc(lines))
    write_output(result)
    check_result(result)
