#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys
from collections import defaultdict, namedtuple
from pprint import pprint

NodesInPath = namedtuple('NodesInPath', ['nodes', 'length'])

class DAG:
    def __init__(self, edges):
        self.edges = edges
        self.dag = self.createDAG(edges)

    def createDAG(self, edges):
        d = defaultdict(list)
        for edge in edges:
            a,b = edge.split(")")
            d[a].append(b)
        return d

    def getGraph(self):
        return self.dag

    def getNodes(self):
        s = set()
        for node in self.dag:
            s.add(node)
            for node2 in self.dag[node]:
                s.add(node2)
        return s

    def countNodesInPath(self, startnode, endnode, nodesInPath):
        if startnode == endnode:
            return NodesInPath(nodesInPath, 1)

        childs = self.dag.get(startnode, None)
        if childs is None:
            return NodesInPath(nodesInPath, 0)

        for child in childs:
            res = self.countNodesInPath(child, endnode, nodesInPath)
            if res.length > 0:
                nodesInPath = res.nodes
                nodesInPath.append(startnode)
                return NodesInPath(nodesInPath, res.length + 1)

        return NodesInPath(nodesInPath, 0)

    def countOrbits(self, node):
        return self.countNodesInPath("COM", node, []).length - 1

    def getAllNodesOrbited(self, node):
        return self.countNodesInPath("COM", node, []).nodes

    def countOrbitalTransfersBetween(self, currentPosition, destination):
        nodesIncurrentPositionPath = self.getAllNodesOrbited(currentPosition)
        nodesdestinationPath = self.getAllNodesOrbited(destination)
        print()
        return len(set(nodesIncurrentPositionPath).symmetric_difference(nodesdestinationPath))


def countTotalNumberOfOrbits(graph):
    return sum(graph.countOrbits(node) for node in graph.getNodes())

def part1(data):
    return countTotalNumberOfOrbits(DAG(data))

def part2(data):
    return DAG(data).countOrbitalTransfersBetween("YOU", "SAN")

## Unit tests ########################################################

class TestDay06(unittest.TestCase):
    graph = DAG(["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"])

    def test_create_graph(self):
        self.assertEqual(self.graph.getGraph(), { "COM" : ["B"],
                                                    "B" : ["C", "G"],
                                                    "C" : ["D"],
                                                    "D" : ["E", "I"],
                                                    "E" : ["F", "J"],
                                                    "G" : ["H"],
                                                    "J" : ["K"],
                                                    "K" : ["L"]})

    def test_count_orbits_for_D(self):
        self.assertEqual(self.graph.countOrbits("D"), 3)

    def test_count_orbits_for_L(self):
        self.assertEqual(self.graph.countOrbits("L"), 7)

    def test_count_orbits_for_I(self):
        self.assertEqual(self.graph.countOrbits("I"), 4)

    def test_count_orbits_for_COM(self):
        self.assertEqual(self.graph.countOrbits("COM"), 0)

    def test_getNodes(self):
        self.assertEqual(self.graph.getNodes(), {"COM","B","C","D","E","F","G","H","I","J","K","L"})

    def test_countTotalNumberOfOrbits(self):
        self.assertEqual(countTotalNumberOfOrbits(self.graph), 42)

class TestDay06_part2(unittest.TestCase):
    graph = DAG(["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN"])

    def test_getAllNodesOrbitedByYou(self):
        self.assertEqual(self.graph.getAllNodesOrbited("YOU"), ["K","J","E","D","C","B","COM"])

    def test_getAllNodesOrbitedBySanta(self):
        self.assertEqual(self.graph.getAllNodesOrbited("SAN"), ["I","D","C","B","COM"])

    def test_countOrbitalTransfersBetween(self):
        self.assertEqual(self.graph.countOrbitalTransfersBetween("YOU", "SAN"), 4)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 6")
    print("Part1 result: {}".format(part1(getStringsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getStringsFromFile(sys.argv[1]))))
