package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	data, _ := adventofcode2017.GetInput("day5.txt")
	fmt.Println(parseOpcode(data, 0))
}

func parseOpcode(str string, wantPos int) (answ int) {
	sArr := strings.Split(str, ",")
	input := make([]int, len(sArr))
	for k, s := range sArr {
		i, _ := strconv.Atoi(s)
		input[k] = i
	}
	for i := 0; ; {
		operation := input[i] % 100

		switch operation {
		case 1:
			num1, num2, pos := getValues(input, i)
			input[pos] = num1 + num2
			i = i + 4
		case 2:
			num1, num2, pos := getValues(input, i)
			input[pos] = num1 * num2
			i = i + 4
		case 3:
			input[input[i+1]] = 5
			i = i + 2
		case 4:
			param1 := 0
			if (input[i]%1000)/100 == 0 {
				param1 = input[input[i+1]]
			} else {
				param1 = input[i+1]
			}
			fmt.Println(param1)
			i = i + 2
		case 5:
			param1, pos := getValuePair(input, i)
			if param1 != 0 {
				i = pos
			} else {
				i = i + 3
			}
		case 6:
			param1, pos := getValuePair(input, i)
			if param1 == 0 {
				i = pos
			} else {
				i = i + 3
			}
		case 7:
			num1, num2, pos := getValues(input, i)
			if num1 < num2 {
				input[pos] = 1
			} else {
				input[pos] = 0
			}
			i = i + 4
		case 8:
			num1, num2, pos := getValues(input, i)
			if num1 == num2 {
				input[pos] = 1
			} else {
				input[pos] = 0
			}
			i = i + 4
		case 99:
			answ = input[wantPos]
			return
		default:
			return -1
		}
	}
	return -1
}

func getValuePair(input []int, i int) (param1, pos int) {
	if (input[i]%1000)/100 == 0 {
		param1 = input[input[i+1]]
	} else {
		param1 = input[i+1]
	}

	if input[i]/1000 == 0 {
		pos = input[input[i+2]]
		//fmt.Println("here", input[i])
	} else {
		pos = input[i+2]
	}
	//fmt.Println("test", param2, pos)
	return
}

func getValues(input []int, i int) (param1, param2, pos int) {
	if (input[i]%1000)/100 == 0 {
		param1 = input[input[i+1]]
	} else {
		param1 = input[i+1]
	}

	if (input[i]%10000)/1000 == 0 {
		param2 = input[input[i+2]]
	} else {
		param2 = input[i+2]
	}

	if input[i]/10000 == 0 {
		pos = input[i+3]
	}

	return
}
