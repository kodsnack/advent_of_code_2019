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
			//num1, num2, pos := input[i+1], input[i+2], input[i+3]
			num1, num2, pos := getValues(input, i)
			//input[pos] = input[num1] + input[num2]
			input[pos] = num1 + num2
			i = i + 4
		case 2:
			//num1, num2, pos := input[i+1], input[i+2], input[i+3]
			num1, num2, pos := getValues(input, i)
			//input[pos] = input[num1] * input[num2]
			input[pos] = num1 * num2
			i = i + 4
		case 3:
			input[input[i+1]] = 1
			i = i + 2
		case 4:
			fmt.Println(input[input[i+1]])
			i = i + 2
		case 99:
			//answ = input[wantPos]
			return
		default:
			return -1
		}
	}
	return -1
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
