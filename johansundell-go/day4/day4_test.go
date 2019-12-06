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

// Part 2 ;)
func Test_Ex7(t *testing.T) {
	if !isValidPasswordPart2(112233) {
		t.Fail()
	}
}

func Test_Ex8(t *testing.T) {
	if isValidPasswordPart2(123444) {
		t.Fail()
	}
}

func Test_Ex9(t *testing.T) {
	if !isValidPasswordPart2(111122) {
		t.Fail()
	}
}

func Test_Ex10(t *testing.T) {
	if !isValidPasswordPart2(113444) {
		t.Fail()
	}
}

func Test_Ex11(t *testing.T) {
	if isValidPasswordPart2(788889) {
		t.Fail()
	}
}

func Test_Ex12(t *testing.T) {
	if isValidPasswordPart2(777888) {
		t.Fail()
	}
}
