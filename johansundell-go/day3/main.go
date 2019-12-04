package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type wiresResult struct {
	wire1Passed, wire2Passed bool
	steps1, steps2           int
}

type point struct {
	x, y int
}

func main() {
	data, _ := adventofcode2017.GetInput("day3.txt")
	arr := strings.Split(data, "\n")
	fmt.Println(getClosestPointDistance(arr[0], arr[1]))
}

func getClosestPointDistance(wire1, wire2 string) (int, int) {
	dist, distSteps := math.MaxInt32, math.MaxInt32
	grid := make(map[point]wiresResult)
	followWire(wire1, 1, grid)
	followWire(wire2, 2, grid)
	for pos, wr := range grid {
		if wr.wire1Passed && wr.wire2Passed {
			d := int(math.Abs(float64(pos.x))) + int(math.Abs(float64(pos.y)))
			if d < dist {
				dist = d
			}
			if wr.steps1+wr.steps2 < distSteps {
				distSteps = wr.steps1 + wr.steps2
			}
		}
	}
	return dist, distSteps
}

func followWire(instructions string, lineType int, grid map[point]wiresResult) {
	arr := strings.Split(instructions, ",")
	currentPos := point{}
	steps := 0
	for _, row := range arr {
		dir := row[0:1]
		length, _ := strconv.Atoi(row[1:])
		for i := 0; i < length; i++ {
			steps++
			switch dir {
			case "R":
				currentPos.x++
			case "U":
				currentPos.y++
			case "L":
				currentPos.x--
			case "D":
				currentPos.y--
			}
			wr := grid[currentPos]
			if lineType == 1 {
				if wr.steps1 == 0 {
					wr.steps1 = steps
				}
				wr.wire1Passed = true
			} else {
				if wr.steps2 == 0 {
					wr.steps2 = steps
				}
				wr.wire2Passed = true
			}
			grid[currentPos] = wr
		}
	}
}
