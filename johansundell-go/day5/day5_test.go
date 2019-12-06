package main

import "testing"

func Test_Ex1(t *testing.T) {
	if parseOpcode("1,9,10,3,2,3,11,0,99,30,40,50", 0) != 3500 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if parseOpcode("1,0,0,0,99", 0) != 2 {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	if parseOpcode("2,3,0,3,99", 3) != 6 {
		t.Fail()
	}
}

func Test_Ex4(t *testing.T) {
	if parseOpcode("2,4,4,5,99,0", 5) != 9801 {
		t.Fail()
	}
}

func Test_Ex5(t *testing.T) {
	str := "1,1,1,4,99,5,6,0,99"
	if parseOpcode(str, 0) != 30 {
		t.Fail()
	}

	if parseOpcode(str, 4) != 2 {
		t.Fail()
	}
}

func Test_Ex6(t *testing.T) {
	if parseOpcode("1002,4,3,4,33", 4) != 99 {
		t.Fail()
	}
}

func Test_Ex7(t *testing.T) {
	if parseOpcode("3,0,4,0,99", 4) != 99 {
		//t.Fail()
	}
}
