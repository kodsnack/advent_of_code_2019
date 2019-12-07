#!/usr/bin/env python3

# By Jakob Ruhe 2019-12-06
# Started early morning, but not completed until I had the time to look at it again in the evening.

import unittest
from collections import namedtuple

Relation = namedtuple("Relation", ["parent", "child"])


class Tree:
    def __init__(self, name, parent = None):
        self.name = name
        self.parent = parent
        self.children = []

    def add_child(self, child):
        self.children.append(child)

    def __str__(self):
        return self.name


def create_relation_from_string(s):
    parent_and_child = s.split(")")
    return Relation(parent_and_child[0], parent_and_child[1])


def parse_input(input: str):
    return list(map(create_relation_from_string, input.strip().split()))


def build_tree(relations):
    nodes = {}
    for r in relations:
        if r.parent in nodes:
            parent = nodes[r.parent]
        else:
            parent = Tree(r.parent)
            nodes[r.parent] = parent
        if r.child not in nodes:
            child = Tree(r.child, parent)
            nodes[r.child] = child
        else:
            child = nodes[r.child]
            if child.parent is None:
                child.parent = parent
            elif child.parent != r.parent:
                ValueError("Child {} already have a parent: {}, new parent: {}"
                           .format(r.child, child.parent.name, r.parent))
        parent.add_child(child)
    return nodes["COM"]


def depth_sum(tree, depth):
    children_depth = 0
    for child in tree.children:
        children_depth += depth_sum(child, depth + 1)
    return depth + children_depth


def solve1(root):
    return depth_sum(root, 0)


def find_node(tree, name):
    if tree.name == name:
        return tree
    for c in tree.children:
        node = find_node(c, name)
        if node is not None:
            return node
    return None


def find_parents(node):
    parents = []
    while node.parent is not None:
        parents.append(node.parent)
        node = node.parent
    return list(reversed(parents))


def solve2(root, from_name, to_name):
    from_node = find_node(root, from_name)
    from_node_parents = find_parents(from_node)
    to_node = find_node(root, to_name)
    to_node_parents = find_parents(to_node)
    num_common = 0
    for i, n in enumerate(to_node_parents):
        if n == from_node_parents[i]:
            num_common += 1
        else:
            break
    return len(from_node_parents) + len(to_node_parents) - num_common * 2


class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(solve1(build_tree(parse_input("COM)A"))), 1)
        self.assertEqual(solve1(build_tree(parse_input("A)B COM)A"))), 3)
        self.assertEqual(solve1(build_tree(parse_input("COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L"))), 42)


if __name__ == "__main__":
    with open("input/day6.txt", "r") as f:
        relations = parse_input(f.read())
    root = build_tree(relations)
    print("P1: {}".format(solve1(root)))
    print("P2: {}".format(solve2(root, "YOU", "SAN")))

