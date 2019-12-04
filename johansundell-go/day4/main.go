package main

import (
	"fmt"
	"strconv"
	"strings"
)

func main() {
	passwords := 0
	passwordsPart2 := 0
	for i := 278384; i < 824795; i++ {
		if isValidPassword(i) {
			passwords++
			if isValidPasswordPart2(i) {
				passwordsPart2++
			}
		}
	}
	fmt.Println(passwords, passwordsPart2)
}

func isValidPassword(num int) bool {
	str := fmt.Sprintf("%d", num)
	parts := strings.Split(str, "")
	hasDouble := false
	for i := 0; i < len(parts)-1; i++ {
		first, _ := strconv.Atoi(strings.Join(parts[i:i+1], ""))
		sec, _ := strconv.Atoi(strings.Join(parts[i+1:i+2], ""))
		if first == sec && !hasDouble {
			hasDouble = true
		}
		if first > sec {
			return false
		}
	}
	return hasDouble
}

func isValidPasswordPart2(num int) bool {
	str := fmt.Sprintf("%d", num)
	parts := strings.Split(str, "")
	m := make(map[int]bool)
	for i := 0; i < len(parts)-1; i++ {
		first, _ := strconv.Atoi(strings.Join(parts[i:i+1], ""))
		sec, _ := strconv.Atoi(strings.Join(parts[i+1:i+2], ""))
		if first == sec {
			v, ok := m[first]
			if !ok {
				m[first] = true
			} else if v {
				m[first] = false
			}
		}
	}
	for _, v := range m {
		if v {
			return true
		}
	}
	return false
}
