package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	data, err := adventofcode2017.GetInput("day1.txt")
	if err != nil {
		panic(err)
	}
	var part1 int64
	for _, v := range parseInput(data) {
		part1 += getMass(v)
	}
	fmt.Println(part1)
}

func parseInput(str string) []string {
	s := strings.Split(str, "\n")
	return s
}

func getMass(str string) int64 {
	weight, _ := strconv.Atoi(str)
	step1 := float64(weight) / 3
	return int64(step1) - 2
}
