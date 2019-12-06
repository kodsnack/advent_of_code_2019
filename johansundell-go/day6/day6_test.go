package main

import "testing"

var input = `COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L`

func Test_Ex1(t *testing.T) {
	if parseInput(input) != 42 {
		t.Fail()
	}
}
