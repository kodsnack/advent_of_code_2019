import math
import os.path
from dataclasses import dataclass
from itertools import combinations


@dataclass
class Moon:
    x: int
    y: int
    z: int
    dx: int = 0
    dy: int = 0
    dz: int = 0


def _parse_moons(lines):
    moons = []
    for line in lines:
        parts = line.replace('<', '').replace('>', '').split(',')
        x = int(parts[0].replace('x=', ''))
        y = int(parts[1].replace('y=', ''))
        z = int(parts[2].replace('z=', ''))
        moons.append(Moon(x, y, z))
    return moons


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        return _parse_moons(f.readlines())


def _apply_gravity(m1: Moon, m2: Moon):
    if m1.x > m2.x:
        m1.dx -= 1
        m2.dx += 1
    elif m1.x < m2.x:
        m1.dx += 1
        m2.dx -= 1

    if m1.y > m2.y:
        m1.dy -= 1
        m2.dy += 1
    elif m1.y < m2.y:
        m1.dy += 1
        m2.dy -= 1

    if m1.z > m2.z:
        m1.dz -= 1
        m2.dz += 1
    elif m1.z < m2.z:
        m1.dz += 1
        m2.dz -= 1


def _potential_energy(moons):
    return [abs(m.x) + abs(m.y) + abs(m.z) for m in moons]


def _kinetic_energy(moons):
    return [abs(m.dx) + abs(m.dy) + abs(m.dz) for m in moons]


def _time_step(moons, pairs):
    for m1, m2 in pairs:
        _apply_gravity(m1, m2)

    for m in moons:
        m.x += m.dx
        m.y += m.dy
        m.z += m.dz


def part1(moons, n=1000):
    pairs = list(combinations(moons, 2))
    for _ in range(n):
        _time_step(moons, pairs)

    return sum([
        p * k for p, k in zip(_potential_energy(moons), _kinetic_energy(moons))
    ])


def _lcm(a, b):
    return abs(a * b) // math.gcd(a, b)


def part2(moons):
    pairs = list(combinations(moons, 2))
    xs = set()
    ys = set()
    zs = set()
    found_x = False
    found_y = False
    found_z = False

    while True:
        x_state = tuple((m.x, m.dx) for m in moons)
        y_state = tuple((m.y, m.dy) for m in moons)
        z_state = tuple((m.z, m.dz) for m in moons)
        if x_state in xs:
            found_x = True
        else:
            xs.add(x_state)

        if y_state in ys:
            found_y = True
        else:
            ys.add(y_state)

        if z_state in zs:
            found_z = True
        else:
            zs.add(z_state)

        if found_x and found_y and found_z:
            break

        _time_step(moons, pairs)

    return _lcm(len(xs), _lcm(len(ys), len(zs)))


if __name__ == '__main__':
    moons = _read_input()
    print(part1(moons))
    moons = _read_input()
    print(part2(moons))


############
# Tests

example_1 = '''\
<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>
'''.splitlines()

example_2 = '''\
<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>
'''.splitlines()


def test_apply_gravity():
    m1 = Moon(3, 0, 4)
    m2 = Moon(5, 0, 3)
    _apply_gravity(m1, m2)
    assert m1.dx == 1
    assert m2.dx == -1
    assert m1.dy == 0
    assert m2.dy == 0
    assert m1.dz == -1
    assert m2.dz == 1


def test_example_1():
    assert part1(_parse_moons(example_1), n=10) == 179
    assert part2(_parse_moons(example_1)) == 2772


def test_example_2():
    assert part1(_parse_moons(example_2), n=100) == 1940
    assert part2(_parse_moons(example_2)) == 4686774924


def test_solutions():
    moons = _read_input()
    assert part1(moons) == 10635
    moons = _read_input()
    assert part2(moons) == 583523031727256
