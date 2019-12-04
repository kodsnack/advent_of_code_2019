from utils import read_input, write_output, check_result
import re


def calc(lines):
    result = 99999999
    parser = re.compile("-?[RLUD]+\d+")
    values = [parser.findall(line.strip()) for line in lines]

    wire_paths = []

    for wire in values:
        x = 0
        y = 0
        wire_path = []

        for vector in wire:
            dir = vector[0]
            len = int(vector[1:])

            if dir == 'R':
                for step in range(len):
                    x += 1
                    wire_path.append((x, y))

            elif dir == 'L':
                for step in range(len):
                    x -= 1
                    wire_path.append((x, y))

            elif dir == 'U':
                for step in range(len):
                    y += 1
                    wire_path.append((x, y))

            elif dir == 'D':
                for step in range(len):
                    y -= 1
                    wire_path.append((x, y))

        wire_paths.append(wire_path)

    intersections = list(set(wire_paths[0]).intersection(wire_paths[1]))

    for intersection in intersections:
        dis = 0
        for wire_path in wire_paths:
            for coordinate in wire_path:
                dis += 1
                if coordinate == intersection:
                    break

        result = min(result, dis)

    return result


if __name__ == '__main__':
    lines = read_input()
    result = str(calc(lines))
    write_output(result)
    check_result(result)
