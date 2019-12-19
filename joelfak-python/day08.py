#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys, itertools

def splitNumber(numberstring):
    l = []
    for char in numberstring:
        l.append(int(char))
    return l

def decode(imagedata, width, height):
    l = splitNumber(imagedata)

    image=[]
    layer = 0
    while len(l) > 0:
        image.append([])
        for y in range(height):
            image[layer].append([])
            for x in range(width):
                image[layer][y].append(l.pop(0))
        layer += 1

    return image

def errorCheck(image):
    minLayer = min(image, key=lambda layer: sum(1 for val in itertools.chain(*layer) if val==0))
    num1digits = sum(1 for val in itertools.chain(*minLayer) if val==1)
    num2digits = sum(1 for val in itertools.chain(*minLayer) if val==2)
    return num1digits * num2digits

def part1(imagedata):
    image = decode(imagedata, 25, 6)
    return errorCheck(image)

def part2(data):
    return 0

## Unit tests ########################################################

class TestDay08_part1(unittest.TestCase):
    def test_splitNumber(self):
        self.assertEqual(splitNumber("278"),[2,7,8])

    def test_decode(self):
        self.assertEqual(decode("123456789012", 3, 2),
                         [[[1,2,3],
                           [4,5,6]],

                          [[7,8,9],
                           [0,1,2]]])

    def test_errorCheck(self):
        image = decode("111223567890", 3, 2)
        self.assertEqual(errorCheck(image), 6)

## Main ########################################################

if __name__ == '__main__':


    print("Advent of code day 8")
    print("Part1 result: {}".format(part1(list(getStringsFromFile(sys.argv[1]))[0])))
    # print("Part2 result: {}".format(part2(getIntsFromFile(sys.argv[1]))))
