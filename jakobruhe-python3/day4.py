#!/usr/bin/env python3

# By Jakob Ruhe 2019-12-04 at around 07:00

import unittest


def has_adjacent_digits(digits):
    for i in range(1, len(digits)):
        if digits[i-1] == digits[i]:
            return True
    return False


def has_2_adjacent_digits(digits):
    num_equal = 1
    for i in range(1, len(digits)):
        if digits[i] == digits[i-1]:
            num_equal += 1
        elif num_equal == 2:
            break
        else:
            num_equal = 1
    return num_equal == 2


def only_same_or_increasing(digits):
    for i in range(1, len(digits)):
        if int(digits[i]) < int(digits[i-1]):
            return False
    return True


def is_valid_password1(code):
    digits = str(code)
    return has_adjacent_digits(digits) and only_same_or_increasing(digits)


def is_valid_password2(code):
    digits = str(code)
    return has_2_adjacent_digits(digits) and only_same_or_increasing(digits)


class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(has_adjacent_digits(str(123)), False)
        self.assertEqual(has_adjacent_digits(str(122)), True)
        self.assertEqual(only_same_or_increasing(str(111)), True)
        self.assertEqual(only_same_or_increasing(str(101)), False)
        self.assertEqual(is_valid_password1(111111), True)
    def test2(self):
        self.assertEqual(has_2_adjacent_digits(str(123)), False)
        self.assertEqual(has_2_adjacent_digits(str(122)), True)
        self.assertEqual(has_2_adjacent_digits(str(1222)), False)
        self.assertEqual(has_2_adjacent_digits(str(12221)), False)
        self.assertEqual(has_2_adjacent_digits(str(221)), True)


if __name__ == "__main__":
    seq = range(235741, 706948+1)
    print("{}".format(sum(map(is_valid_password1, seq))))
    print("{}".format(sum(map(is_valid_password2, seq))))
