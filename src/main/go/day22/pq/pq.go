package pq

import (
	"container/heap"
	"fmt"
)

type Point struct {
	X, Y int
}

func (p Point) East() Point {
	return Point{p.X - 1, p.Y}
}
func (p Point) West() Point {
	return Point{p.X + 1, p.Y}
}
func (p Point) North() Point {
	return Point{p.X, p.Y - 1}
}
func (p Point) South() Point {
	return Point{p.X, p.Y + 1}
}

type RType int

const (
	Rocky RType = iota
	Wet
	Narrow
)

type Tool int

const (
	Torch Tool = iota
	Climbing
	Neither
)

func (t Tool) String() string {
	tools := []string{"Torch", "Climbing", "Neither"}
	return tools[t]
}

type Region struct {
	P Point
	T Tool
}

type RegionWithDistance struct {
	R Region
	D, Index int
}

func (r Region) String() string {
	return fmt.Sprintf("Point (%d, %d), Tool: %s", r.P.X, r.P.Y, r.T.String())
}

type PriorityQueue []*RegionWithDistance

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	return pq[i].D < pq[j].D
	//We want Pop to give us the highest, not lowest, priority so we use greater than here.
	//ip := pq[i].P.X + pq[i].P.Y
	//jp := pq[j].P.X + pq[j].P.Y
	//return ip < jp
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].Index = i
	pq[j].Index = j
}

func (pq *PriorityQueue) Push(x interface{}) {
	n := len(*pq)
	item := x.(*RegionWithDistance)
	item.Index = n
	*pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	item.Index = -1 // for safety
	*pq = old[0 : n-1]
	return item
}

func (pq *PriorityQueue) Update(item *RegionWithDistance, distance int) {
	item.D = distance
	heap.Fix(pq, item.Index)
}