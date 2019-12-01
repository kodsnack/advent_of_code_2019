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
	var part1, part2 int64
	for _, v := range parseInput(data) {
		i := getMass(v)
		part2 += getFuel(i)
		part1 += i

	}
	fmt.Println(part1, part2)
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

func getFuel(mass int64) (fuel int64) {
	fuel = mass
	for i := int64(float64(mass)/3) - 2; fuel > 0 && i > 0; i = int64(float64(i)/3) - 2 {
		fuel += i
	}
	return
}
