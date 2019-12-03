def absdist(coords):
    return sum(abs(x) for x in coords)


def coord_diff(a, b):
    return [a[x] - b[x] for x in range(len(a))]


def manhattan(a, b):
    return absdist(coord_diff(a, b))