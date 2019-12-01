package main

import "testing"

func Test_Ex1(t *testing.T) {
	if getMass("12") != 2 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if getMass("14") != 2 {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	if getMass("1969") != 654 {
		t.Fail()
	}
}

func Test_Ex4(t *testing.T) {
	if getMass("100756") != 33583 {
		t.Fail()
	}
}
