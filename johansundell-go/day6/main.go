package main

import (
	"fmt"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type orbitMap map[string]string

func (o orbitMap) getParents(str string) (i int) {
	for row, ok := o[str]; ok; row, ok = o[row] {
		i++
	}
	return
}

func (o orbitMap) add(name, parent string) {
	name, parent = strings.TrimSpace(name), strings.TrimSpace(parent)
	o[name] = parent
}

func main() {
	data, _ := adventofcode2017.GetInput("day6.txt")
	fmt.Println(getTotalOrbits(data), getDistance(data, "YOU", "SAN"))
}

func parseInput(input string) orbitMap {
	data := strings.Split(input, "\n")
	om := make(orbitMap)
	for _, row := range data {
		ab := strings.Split(row, ")")
		om.add(ab[1], ab[0])
	}
	return om
}

func getTotalOrbits(input string) (tot int) {
	om := parseInput(input)
	for k := range om {
		tot += om.getParents(k)
	}
	return
}

func getDistance(input, from, to string) (tot int) {
	om := parseInput(input)
	dists := make(map[string]int)
	for row, ok := om[from]; ok; row, ok = om[row] {
		dists[row] = len(dists)
	}
	for row, ok := om[to]; ok; row, ok = om[row] {
		if d, found := dists[row]; found {
			tot = tot + d
			break
		}
		tot++
	}
	return
}
