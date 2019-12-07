import os.path
from collections import defaultdict

from graph import djikstra


def _parse_orbits(lines):
    orbits = {}
    for line in lines:
        a, b = line.strip().split(')')
        if b in orbits:
            raise ValueError(f'object {b} orbits around more than one object')
        orbits[b] = a
    return orbits


def _build_neighbor_map(orbits):
    neighbors = defaultdict(list)
    for k, v in orbits.items():
        neighbors[k].append(v)
        neighbors[v].append(k)
    return neighbors


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        return _parse_orbits(f.readlines())


def _count(obj, orbits):
    if obj not in orbits or obj == 'COM':
        return 0
    return 1 + _count(orbits[obj], orbits)


def part1(orbits):
    n = 0
    for k in orbits.keys():
        n += _count(k, orbits)
    return n


def part2(neighbors):
    dist, _ = djikstra(neighbors, target='SAN', source='YOU')
    return dist['SAN'] - 2  # subtract 2 for source and target node


orbits = _read_input()
neighbors = _build_neighbor_map(orbits)
print(part1(orbits))
print(part2(neighbors))


############
# Tests

# fmt: off
example = _parse_orbits('''COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
'''.splitlines())

example2 = _parse_orbits('''COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN
'''.splitlines())
example_neighbors = _build_neighbor_map(example2)
# fmt: on


def test_count():
    assert _count('COM', example) == 0
    assert _count('D', example) == 3
    assert _count('L', example) == 7


def test_neighbour_map():
    assert _build_neighbor_map(example)['COM'] == ['B']
    assert sorted(_build_neighbor_map(example)['D']) == ['C', 'E', 'I']
    assert sorted(_build_neighbor_map(example)['B']) == ['C', 'COM', 'G']


def test_example():
    assert part1(example) == 42
    assert part2(example_neighbors) == 4


def test_solution():
    assert part1(orbits) == 300598
    assert part2(neighbors) == 520
