import os.path


def _parse_row(row):
    return [(x[:1], int(x[1:])) for x in row.split(',')]


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        w1 = _parse_row(f.readline())
        w2 = _parse_row(f.readline())
    return w1, w2


def _path2coords(path):
    coords = [(0, 0)]
    for d, count in path:
        x, y = coords[-1]
        # fmt: off
        if d == 'U':
            coords.extend(
                zip([x] * count, range(y - 1, y - count - 1, -1))
            )
        elif d == 'R':
            coords.extend(
                zip(range(x + 1, x + count + 1), [y] * count)
            )
        elif d == 'D':
            coords.extend(
                zip([x] * count, range(y + 1, y + count + 1))
            )
        elif d == 'L':
            coords.extend(
                zip(range(x - 1, x - count - 1, -1), [y] * count)
            )
        # fmt: on
    return coords


def _intersects(c1, c2):
    return set(c1) & set(c2)


def _closest_intersect(intersections):
    dists = []
    for x, y in intersections:
        dists.append(abs(x) + abs(y))
    return min(dists)


def part1(w1, w2):
    p1 = _path2coords(w1)
    p2 = _path2coords(w2)
    intersections = _intersects(p1, p2)
    intersections.remove((0, 0))
    return _closest_intersect(intersections)


def part2(w1, w2):
    p1 = _path2coords(w1)
    p2 = _path2coords(w2)
    intersections = _intersects(p1, p2)
    intersections.remove((0, 0))
    scores = {}
    for i in intersections:
        score = p1.index(i) + p2.index(i)
        if i not in scores or i in scores and scores[i] > score:
            scores[i] = score

    return min(scores.items(), key=lambda x: x[1])[1]


w1, w2 = _read_input()
print(part1(w1, w2))
print(part2(w1, w2))


############
# Tests


def test_path2coords():
    assert _path2coords(_parse_row('U3')) == [(0, 0), (0, -1), (0, -2), (0, -3)]
    assert _path2coords(_parse_row('U1')) == [(0, 0), (0, -1)]
    assert _path2coords(_parse_row('U0')) == [(0, 0)]
    assert _path2coords(_parse_row('R2')) == [(0, 0), (1, 0), (2, 0)]
    assert _path2coords(_parse_row('R4')) == [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]
    assert _path2coords(_parse_row('R1')) == [(0, 0), (1, 0)]
    assert _path2coords(_parse_row('R0')) == [(0, 0)]
    assert _path2coords(_parse_row('D3')) == [(0, 0), (0, 1), (0, 2), (0, 3)]
    assert _path2coords(_parse_row('D0')) == [(0, 0)]
    assert _path2coords(_parse_row('D1')) == [(0, 0), (0, 1)]
    assert _path2coords(_parse_row('L2')) == [(0, 0), (-1, 0), (-2, 0)]
    assert _path2coords(_parse_row('L0')) == [(0, 0)]
    assert _path2coords(_parse_row('L4')) == [
        (0, 0),
        (-1, 0),
        (-2, 0),
        (-3, 0),
        (-4, 0),
    ]

    assert _path2coords(_parse_row('U2,R2')) == [
        (0, 0),
        (0, -1),
        (0, -2),
        (1, -2),
        (2, -2),
    ]

    ex1 = _parse_row('R8,U5,L5,D3')
    ex2 = _parse_row('U7,R6,D4,L4')
    assert len(_path2coords(ex1)) == 22
    assert len(_path2coords(ex2)) == 22


def test_intersects():
    ex1 = _parse_row('R8,U5,L5,D3')
    ex2 = _parse_row('U7,R6,D4,L4')
    p1 = _path2coords(ex1)
    p2 = _path2coords(ex2)
    i = _intersects(p1, p2)
    assert i == set([(0, 0), (3, -3), (6, -5)])
    i.remove((0, 0))
    assert _closest_intersect(i) == 6
    i.remove((3, -3))
    assert _closest_intersect(i) == 11


def test_examples():
    ex1 = _parse_row('R75,D30,R83,U83,L12,D49,R71,U7,L72')
    ex2 = _parse_row('U62,R66,U55,R34,D71,R55,D58,R83')
    assert part1(ex1, ex2) == 159
    assert part2(ex1, ex2) == 610

    ex1 = _parse_row('R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51')
    ex2 = _parse_row('U98,R91,D20,R16,D67,R40,U7,R15,U6,R7')
    assert part1(ex1, ex2) == 135
    assert part2(ex1, ex2) == 410


def test_solutions():
    part1(w1, w2) == 709
    part2(w1, w2) == 13836
