package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	data, _ := adventofcode2017.GetInput("day2.txt")
	fmt.Println(parseOpcode(modifyInput(data, 12, 2), 0), search(data))
}

func parseOpcode(str string, wantPos int) int {
	sArr := strings.Split(str, ",")
	input := make([]int, len(sArr))
	for k, s := range sArr {
		i, _ := strconv.Atoi(s)
		input[k] = i
	}
	for i := 0; ; i = i + 4 {
		operation := input[i]
		if operation == 99 {
			break
		}
		num1, num2, pos := input[i+1], input[i+2], input[i+3]
		switch operation {
		case 1:
			input[pos] = input[num1] + input[num2]
		case 2:
			input[pos] = input[num1] * input[num2]
		default:
			return -1
		}
	}
	return input[wantPos]
}

func modifyInput(str string, noun, verb int) string {
	sArr := strings.Split(str, ",")
	sArr[1] = strconv.Itoa(noun)
	sArr[2] = strconv.Itoa(verb)
	return strings.Join(sArr, ",")
}

func search(str string) int {
	for noun := 0; noun <= 99; noun++ {
		for verb := 0; verb <= 99; verb++ {
			switch {
			case parseOpcode(modifyInput(str, noun, verb), 0) == 19690720:
				return 100*noun + verb
			}
		}
	}
	return -1
}
