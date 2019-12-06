package main

import "testing"

func Test_Ex1(t *testing.T) {
	if getTotalOrbits(input) != 42 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if getDistance(input2, "YOU", "SAN") != 4 {
		t.Fail()
	}
}

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

var input2 = `COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN`
