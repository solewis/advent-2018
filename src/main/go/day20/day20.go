package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	dat, _ := ioutil.ReadFile("src/main/go/day20/input.txt")
	fmt.Println("Part 1:", p1(string(dat)))
	fmt.Println("Part 2:", p2(string(dat), 1000))
}

func p1(input string) int {
	d := distances(input)
	max := 0
	for _, d := range d {
		if d > max {
			max = d
		}
	}
	return max
}

func p2(input string, limit int) int {
	d := distances(input)
	c := 0
	for _, d := range d {
		if d >= limit {
			c++
		}
	}
	return c
}

type point struct {
	x, y int
}

func (p point) east() point {
	return point{p.x - 1, p.y}
}
func (p point) west() point {
	return point{p.x + 1, p.y}
}
func (p point) north() point {
	return point{p.x, p.y - 1}
}
func (p point) south() point {
	return point{p.x, p.y + 1}
}

func distances(input string) map[point]int {
	d := make(map[point]int)
	branchPoints := make([]point, 0)
	currentPosition := point{0, 0}
	for _, c := range input {
		currentDistance := d[currentPosition]
		switch c {
		case 'E':
			currentPosition = currentPosition.west()
		case 'W':
			currentPosition = currentPosition.east()
		case 'N':
			currentPosition = currentPosition.north()
		case 'S':
			currentPosition = currentPosition.south()
		case '(':
			branchPoints = append(branchPoints, currentPosition)
		case '|':
			currentPosition = branchPoints[len(branchPoints)-1]
		case ')':
			end := len(branchPoints) - 1
			currentPosition = branchPoints[end]
			branchPoints = branchPoints[:end]
		}
		if _, pointSeen := d[currentPosition]; !pointSeen {
			d[currentPosition] = currentDistance + 1
		}
	}
	return d
}