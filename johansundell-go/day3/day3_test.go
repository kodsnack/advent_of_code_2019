package main

import "testing"

func Test_Ex1(t *testing.T) {
	if a, b := getClosestPointDistance("R8,U5,L5,D3", "U7,R6,D4,L4"); a != 6 || b != 30 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if a, b := getClosestPointDistance("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"); a != 159 || b != 610 {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	if a, b := getClosestPointDistance("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"); a != 135 || b != 410 {
		t.Fail()
	}
}
