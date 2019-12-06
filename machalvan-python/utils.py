def read_input():
    input_file = open("../../input.txt", "r")
    lines = input_file.readlines()
    input_file.close()
    return lines


def write_output(result):
    output_file = open("../../output.txt", "w")
    output_file.write(result)
    output_file.close()


def check_result(result):
    answer_file = open("../../answer.txt", "r")
    answer = answer_file.read()

    if result == answer:
        print('CORRECT')
    else:
        if answer:
            print('EXPECTED')
            print(answer)
            print('ACTUAL')
            print(result)
        else:
            print('GUESS')
            print(result)

    answer_file.close()
