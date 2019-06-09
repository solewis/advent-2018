package main

import (
	"container/heap"
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	nanobots := parse("src/main/go/day23/slow_input.txt")
	strongest := strongest(nanobots)
	inRange := inRangeCount(strongest, nanobots)
	fmt.Println("Part 1:", inRange)

	tp := teleportPoint(nanobots)
	fmt.Println("Part 2:", tp.distanceAway(coordinate{0, 0, 0}))
}

type PriorityQueue []*searchCube

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	if pq[i].bots != pq[j].bots {
		return pq[i].bots > pq[j].bots
	}
	origin := coordinate{0, 0, 0}
	if pq[i].c.min.distanceFrom(origin) != pq[j].c.min.distanceFrom(origin) {
		return pq[i].c.min.distanceFrom(origin) < pq[j].c.min.distanceFrom(origin)
	}
	return pq[i].c.length < pq[j].c.length
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
}

func (pq *PriorityQueue) Push(x interface{}) {
	*pq = append(*pq, x.(*searchCube))
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	*pq = old[0 : n-1]
	return item
}

type coordinate struct {
	x, y, z int
}

func (c coordinate) distanceFrom(o coordinate) int {
	abs := func(a int) int {
		if a < 0 {
			return -a
		}
		return a
	}
	xRange := abs(c.x - o.x)
	yRange := abs(c.y - o.y)
	zRange := abs(c.z - o.z)
	return xRange + yRange + zRange
}

type nanobot struct {
	c      coordinate
	radius int
}

type searchCube struct {
	c    cube
	bots int
}

type cube struct {
	min    coordinate //bottom left front most corner
	length int
}

func (c cube) split() []cube {
	newLength := c.length / 2
	return []cube{
		{coordinate{x: c.min.x, y: c.min.y, z: c.min.z}, newLength},
		{coordinate{x: c.min.x, y: c.min.y, z: c.min.z + newLength}, newLength},
		{coordinate{x: c.min.x, y: c.min.y + newLength, z: c.min.z}, newLength},
		{coordinate{x: c.min.x, y: c.min.y + newLength, z: c.min.z + newLength}, newLength},
		{coordinate{x: c.min.x + newLength, y: c.min.y, z: c.min.z}, newLength},
		{coordinate{x: c.min.x + newLength, y: c.min.y, z: c.min.z + newLength}, newLength},
		{coordinate{x: c.min.x + newLength, y: c.min.y + newLength, z: c.min.z}, newLength},
		{coordinate{x: c.min.x + newLength, y: c.min.y + newLength, z: c.min.z + newLength}, newLength},
	}
}

func (c cube) botsInRange(nanobots []nanobot) int {
	ct := 0
	for _, n := range nanobots {
		if c.distanceAway(n.c) <= n.radius {
			ct++
		}
	}
	return ct
}

func (c cube) distanceAway(crd coordinate) int {
	dist := func(crdVal, cubeVal int) int {
		if crdVal < cubeVal {
			return cubeVal - crdVal
		}
		if crdVal > (cubeVal + c.length - 1) {
			return crdVal - (cubeVal + c.length - 1)
		}
		return 0
	}
	xD := dist(crd.x, c.min.x)
	yD := dist(crd.y, c.min.y)
	zD := dist(crd.z, c.min.z)

	return xD + yD + zD
}

func teleportPoint(nanobots []nanobot) cube {
	c := initialCube(nanobots)
	pq := make(PriorityQueue, 0)
	sc := searchCube{c, len(nanobots)}
	heap.Push(&pq, &sc)

	for {
		current := heap.Pop(&pq).(*searchCube)
		if current.c.length == 1 {
			return current.c
		}
		splitCubes := current.c.split()
		for _, splitCube := range splitCubes {
			sc := searchCube{splitCube, splitCube.botsInRange(nanobots)}
			heap.Push(&pq, &sc)
		}
	}
}

func initialCube(nanobots []nanobot) cube {
	max := func(a, b int) int {
		if a > b {
			return a
		}
		return b
	}
	min := func(a, b int) int {
		if a < b {
			return a
		}
		return b
	}

	minX := nanobots[0].c.x
	maxX := nanobots[0].c.x
	minY := nanobots[0].c.y
	maxY := nanobots[0].c.y
	minZ := nanobots[0].c.z
	maxZ := nanobots[0].c.z
	for _, n := range nanobots {
		minX = min(n.c.x, minX)
		maxX = max(n.c.x, maxX)
		minY = min(n.c.y, minY)
		maxY = max(n.c.y, maxY)
		minZ = min(n.c.z, minZ)
		maxZ = max(n.c.z, maxZ)
	}

	minLength := max(max(maxZ-minZ, maxX-minX), maxY-minY)

	length := 1
	for length < minLength {
		length *= 2
	}
	return cube{coordinate{minX, minY, minZ}, length}
}

func inRangeCount(src nanobot, all []nanobot) int {
	ct := 0
	for _, n := range all {
		if src.c.distanceFrom(n.c) <= src.radius {
			ct++
		}
	}
	return ct
}

func strongest(nanobots []nanobot) nanobot {
	var strongest nanobot
	for _, n := range nanobots {
		if n.radius > strongest.radius {
			strongest = n
		}
	}
	return strongest
}

func parse(filename string) []nanobot {
	dat, err := ioutil.ReadFile(filename)
	check(err)
	lines := strings.Split(string(dat), "\n")

	nanobots := make([]nanobot, len(lines))
	re := regexp.MustCompile(`pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)`)
	for i, l := range lines {
		matches := re.FindStringSubmatch(l)
		x, _ := strconv.Atoi(matches[1])
		y, _ := strconv.Atoi(matches[2])
		z, _ := strconv.Atoi(matches[3])
		radius, _ := strconv.Atoi(matches[4])
		nanobots[i] = nanobot{c: coordinate{x: x, y: y, z: z}, radius: radius}
	}
	return nanobots
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
