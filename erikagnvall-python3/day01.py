import os.path


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        return [int(l) for l in f]


def _m2f(m):
    return m // 3 - 2


def part1(masses):
    return sum(_m2f(m) for m in masses)


def part2(modules):
    total = 0
    for m in modules:
        f = _m2f(m)
        while f > 0:
            total += f
            f = _m2f(f)
    return total


def _mod2f(mass):
    fuel = _m2f(mass)
    if fuel <= 0:
        return 0
    return fuel + _mod2f(fuel)


def part2_rec(modules):
    total = 0
    for m in modules:
        total += _mod2f(m)
    return total


masses = _read_input()
print(part1(masses))
print(part2(masses))
print(f'{part2_rec(masses)} (recursive)')


############
# Tests


def test_m2f():
    assert _m2f(12) == 2
    assert _m2f(14) == 2
    assert _m2f(1969) == 654
    assert _m2f(100756) == 33583


def test_mod2f():
    assert _mod2f(14) == 2
    assert _mod2f(1969) == 966
    assert _mod2f(100756) == 50346


def test_part2():
    assert part2([14]) == 2
    assert part2([1969]) == 966
    assert part2([100756]) == 50346


def test_time_part2():
    import timeit
    t = timeit.Timer('part2(masses)', globals=globals())
    n, s = t.autorange()
    print(f'Itererative: {s/n*1000:.5f}ms')


def test_time_part2_rec():
    import timeit
    t = timeit.Timer('part2_rec(masses)', globals=globals())
    n, s = t.autorange()
    print(f'Recursive: {s/n*1000:.5f}ms')


def test_solution_part1():
    assert part1(masses) == 3216868


def test_solution_part2():
    assert part2(masses) == 4822435
    assert part2_rec(masses) == 4822435
