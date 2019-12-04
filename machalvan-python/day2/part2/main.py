from utils import read_input, write_output, check_result


def calc(lines):
    for noun in range(0, 100):
        for verb in range(0, 100):
            words = lines.split(',')
            pointer = 0

            words[1] = noun
            words[2] = verb

            while pointer < len(words):
                opcode = int(words[pointer])
                if opcode == 1:
                    pointer += 1
                    parameter = int(words[pointer])
                    value1 = int(words[parameter])

                    pointer += 1
                    parameter = int(words[pointer])
                    value2 = int(words[parameter])

                    sum = value1 + value2
                    pointer += 1
                    parameter = int(words[pointer])
                    words[parameter] = sum

                elif opcode == 2:
                    pointer += 1
                    parameter = int(words[pointer])
                    value1 = int(words[parameter])

                    pointer += 1
                    parameter = int(words[pointer])
                    value2 = int(words[parameter])

                    sum = value1 * value2
                    pointer += 1
                    parameter = int(words[pointer])
                    words[parameter] = sum

                elif opcode == 99:
                    break

                pointer += 1

            if int(words[0]) == 19690720:
                return 100 * noun + verb

    return None


if __name__ == '__main__':
    lines = read_input()
    result = str(calc(lines))
    write_output(result)
    check_result(result)
