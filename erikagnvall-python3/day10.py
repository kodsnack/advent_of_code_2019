import math
import os.path
from collections import defaultdict, namedtuple
from operator import itemgetter


Point = namedtuple('Point', ['x', 'y'])


def _parse_asteroids(lines):
    asteroids = set()
    for y, line in enumerate(lines):
        for x, v in enumerate(line.strip()):
            if v == '#':
                asteroids.add(Point(x, y))
    return asteroids


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        return _parse_asteroids(f.readlines())


def _dist(a0, a1):
    # fmt: off
    return math.sqrt((a1.x - a0.x)**2 + (a1.y - a0.y)**2)
    # fmt: on


def _calc_angles(station, asteroids):
    angles = defaultdict(list)
    for a in asteroids:
        if a == station:
            continue

        # fmt: off
        alpha = math.atan2((a.y - station.y), (a.x - station.x)) + math.pi/2
        alpha = alpha + 2*math.pi if alpha < 0 else alpha
        # fmt: on

        angles[round(math.degrees(alpha), 5)].append(a)

    def _dist_to_station(x):
        return _dist(station, x)

    for _, v in angles.items():
        # In-place sorting, yuck.
        v.sort(key=_dist_to_station)

    return angles


def part1(asteroids):
    def _visible(a):
        return len(_calc_angles(a, asteroids).keys())

    station = max(asteroids, key=_visible)
    n = len(_calc_angles(station, asteroids).keys())
    return (station, n)


def part2(station, asteroids):
    angles = _calc_angles(station, asteroids)

    done = False
    n = 0
    while not done:
        done = True
        for a, l in sorted(angles.items(), key=itemgetter(0)):
            if l:
                done = False
                p = l.pop(0)
                n += 1
                if n == 200:
                    return p.y + p.x * 100


asteroids = _read_input()
station, n = part1(asteroids)
print(n)
print(part2(station, asteroids))


############
# Tests

# fmt: off

example_1 = '''\
.#..#
.....
#####
....#
...##
'''

example_2 = _parse_asteroids('''\
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
'''.splitlines())

example_3 = _parse_asteroids('''\
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
'''.splitlines())

example_4 = _parse_asteroids('''\
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
'''.splitlines())

example_big = _parse_asteroids('''\
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
'''.splitlines())

# fmt: on


def test_parse_asteroids():
    asteroids = _parse_asteroids(example_1.splitlines())
    assert Point(1, 0) in asteroids
    assert Point(4, 0) in asteroids
    assert Point(0, 2) in asteroids
    assert Point(1, 2) in asteroids
    assert Point(2, 2) in asteroids
    assert Point(3, 2) in asteroids
    assert Point(4, 2) in asteroids
    assert Point(4, 3) in asteroids
    assert Point(3, 4) in asteroids
    assert Point(4, 4) in asteroids


def test_example_1():
    assert part1(_parse_asteroids(example_1.splitlines())) == (Point(3, 4), 8)


def test_example_2():
    assert part1(example_2) == (Point(5, 8), 33)


def test_example_3():
    assert part1(example_3) == (Point(1, 2), 35)


def test_example_4():
    assert part1(example_4) == (Point(6, 3), 41)


def test_example_big():
    assert part1(example_big) == (Point(11, 13), 210)


def test_example_1_p2():
    assert part2(Point(11, 13), example_big) == 802


def test_solutions():
    assert part1(asteroids) == (Point(28, 29), 340)
    assert part2(Point(28, 29), asteroids) == 2628
