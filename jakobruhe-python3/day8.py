#!/usr/bin/env python3

# By Jakob Ruhe 2019-12-08
# Started at 06:50
# P1 solved at 07:23
# P2 solved at 07:54

import unittest


def split(data, chunk_size):
    assert(len(data) % chunk_size == 0)
    num_chunks = len(data) // chunk_size
    return [data[i*chunk_size:(i+1)*chunk_size] for i in range(num_chunks)]


def find_layer_with_fewest_zeros(layers):
    num_zeros = [layer.count("0") for layer in layers]
    return min(layers, key=lambda layer: layer.count("0"))


def solve1(digits, width, height):
    layers = split(digits, width * height)
    best_layer = find_layer_with_fewest_zeros(layers)
    return best_layer.count("1") * best_layer.count("2")


def merge_layers(layers):
    result = ["."] * len(layers[0])
    for p in range(len(layers[0])):
        for layer in layers:
            if layer[p] == "0":
                result[p] = " "
                break
            elif layer[p] == "1":
                result[p] = "o"
                break
    return "".join(result)


def print_layer(layer, width):
    for row in split(layer, width):
        print(row)


def solve2(digits, width, height):
    layers = split(digits, width * height)
    return merge_layers(layers)


# Execute tests with:
# python3 -m unittest dayX.py
class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(solve1("123456789011", 3, 2), 1)


if __name__ == "__main__":
    with open("input/day8.txt", "r") as f:
        input = f.read().rstrip()
    width, height = 25, 6
    print("P1: {}".format(solve1(input, width, height)))
    print("P2:")
    print_layer(solve2(input, width, height), width)
