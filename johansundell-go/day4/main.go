package main

import (
	"fmt"
	"strconv"
	"strings"
)

func main() {
	passwords := make([]int, 0)
	for i := 278384; i < 824795; i++ {
		if isValidPassword(i) {
			passwords = append(passwords, i)
		}
	}
	fmt.Println(len(passwords))
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
