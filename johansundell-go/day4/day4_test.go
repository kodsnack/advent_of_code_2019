package main

import "testing"

func Test_Ex1(t *testing.T) {
	if !isValidPassword(122345) {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if !isValidPassword(111123) {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	if isValidPassword(135679) {
		t.Fail()
	}
}

func Test_Ex4(t *testing.T) {
	if !isValidPassword(111111) {
		t.Fail()
	}
}

func Test_Ex5(t *testing.T) {
	if isValidPassword(223450) {
		t.Fail()
	}
}

func Test_Ex6(t *testing.T) {
	if isValidPassword(123789) {
		t.Fail()
	}
}
