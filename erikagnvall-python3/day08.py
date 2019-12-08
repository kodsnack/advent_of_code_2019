import os.path
from collections import Counter


def _read_input():
    with open(os.path.basename(__file__).replace('.py', '.txt')) as f:
        return tuple(int(i) for i in f.readline().strip())


def _split_layers(image, w, h):
    layers = []
    layer_size = w * h
    n_layers = len(image) // layer_size
    assert len(image) % layer_size == 0
    for i in range(n_layers):
        layers.append(image[i * layer_size:(i + 1) * layer_size])
    return layers


def _count_digit(layer, digit=0):
    c = Counter(layer)
    return c[digit]


def part1(image, w=25, h=6):
    layers = _split_layers(image, w, h)
    min_zeroes = min(layers, key=_count_digit)
    return _count_digit(min_zeroes, digit=1) * _count_digit(min_zeroes, digit=2)


def _get_color(layers, x, y, width):
    for layer in layers:
        pixel = layer[y * width + x]
        if pixel == 1:
            return 1
        elif pixel == 0:
            return 0
    return 2


def part2(image, w=25, h=6, output=True):
    layers = _split_layers(image, w, h)
    final_image = [3 for _ in range(w * h)]
    for y in range(h):
        for x in range(w):
            pixel = _get_color(layers, x, y, w)
            final_image[y * w + x] = pixel
            print('#' if pixel == 1 else ' ', end='')
        print()
    return final_image


image = _read_input()
print(part1(image))
part2(image)


############
# Tests


def test_split_layers():
    image = tuple(int(i) for i in '123456789012')
    assert _split_layers(image, 3, 2) == [(1, 2, 3, 4, 5, 6), (7, 8, 9, 0, 1, 2)]


def test_count_digits():
    assert _count_digit([1, 2, 3, 1, 0, 2, 1], 0) == 1
    assert _count_digit([1, 2, 3, 1, 0, 2, 1], 1) == 3
    assert _count_digit([1, 2, 3, 1, 0, 2, 1], 2) == 2
    assert _count_digit([1, 2, 3, 1, 0, 2, 1], 3) == 1
    assert _count_digit([1, 2, 3, 1, 0, 2, 1], 4) == 0


def test_solutions():
    assert part1(image) == 2286

    final_image = part2(image)
    # fmt: off
    assert final_image == [
        0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0,
        1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0,
        1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0,
        1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0,
        1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0,
    ]
