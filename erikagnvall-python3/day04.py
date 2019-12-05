import os.path


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        parts = f.readline().split('-')
        start, end = int(parts[0]), int(parts[1])
    return start, end


def _get_adjacent(n):
    has_run = False
    runs = {}
    while n:
        x = n % 10
        x_n = (n % 100) // 10
        if not has_run and x == x_n:  # start of new run
            has_run = True
            runs[x] = 2
        elif has_run and x == x_n:  # continuation of run
            runs[x] += 1
        elif has_run:  # end of run
            has_run = False
        n //= 10

    return runs


def _has_adjacent(n):
    return any(v >= 2 for v in _get_adjacent(n).values())


def _has_adjacent2(n):
    return any(v == 2 for v in _get_adjacent(n).values())


def _increasing(n):
    n = str(n)
    return n == ''.join(sorted(n))


def part1(start, end):
    return len(
        [n for n in range(start, end + 1) if _has_adjacent(n) and _increasing(n)]
    )


def part2(start, end):
    return len(
        [n for n in range(start, end + 1) if _has_adjacent2(n) and _increasing(n)]
    )


start, end = _read_input()
print(part1(start, end))
print(part2(start, end))


############
# Tests


def test_has_adjacent():
    assert _has_adjacent(122345)
    assert not _has_adjacent(123789)
    assert _has_adjacent(111111)


def test_get_adjacent():
    assert _get_adjacent(122345) == {2: 2}
    assert _get_adjacent(123789) == {}
    assert _get_adjacent(111111) == {1: 6}
    assert _get_adjacent(111122) == {1: 4, 2: 2}
    assert _get_adjacent(123321) == {3: 2}
    assert _get_adjacent(123444) == {4: 3}


def test_has_adjacent2():
    assert _has_adjacent2(122345)
    assert not _has_adjacent2(123789)
    assert not _has_adjacent2(111111)
    assert _has_adjacent2(111122)
    assert _has_adjacent2(123321)
    assert not _has_adjacent2(123444)


def test_increasing():
    assert _increasing(122345)
    assert not _increasing(132345)
    assert _increasing(111111)


def test_solutions():
    assert part1(start, end) == 1653
    assert part2(start, end) == 1133
