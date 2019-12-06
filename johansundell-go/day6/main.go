package main

import (
	"fmt"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type orbitMap map[string]string

func (o orbitMap) getParents(str string) (i int) {
	for row, ok := o[str]; ok; i++ {
		row, ok = o[row]
	}
	return
}

func (o orbitMap) add(name, parent string) {
	name, parent = strings.TrimSpace(name), strings.TrimSpace(parent)
	o[name] = parent
}

func parseInput(input string) (tot int) {
	data := strings.Split(input, "\n")
	om := make(orbitMap)
	for _, row := range data {
		ab := strings.Split(row, ")")
		om.add(ab[1], ab[0])
	}
	for k, _ := range om {
		tot += om.getParents(k)
	}
	return
}

func main() {
	data, _ := adventofcode2017.GetInput("day6.txt")
	fmt.Println(parseInput(data))
}
