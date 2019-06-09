package main

import (
	"advent-2018/src/main/go/day22/pq"
	"container/heap"
	"fmt"
)

func main() {
	risk := calculateRisk(9465, pq.Point{13, 704})
	fmt.Println("Part 1:", risk)
	shortestPath := calculateShortestPath(9465, pq.Point{13, 704})
	fmt.Println("Part 2:", shortestPath)
}

func calculateRisk(depth int, target pq.Point) int {
	totalRisk := 0
	erosionCache := make(map[pq.Point]int)
	for y := 0; y <= target.Y; y++ {
		for x := 0; x <= target.X; x++ {
			totalRisk += regionRisk(depth, pq.Point{x, y}, target, erosionCache)
		}
	}
	return totalRisk
}

func calculateShortestPath(depth int, target pq.Point) int {
	erosionCache := make(map[pq.Point]int)
	completeRegions := make(map[pq.Region]int)
	toVisit := make(pq.PriorityQueue, 0)
	regionMap := make(map[pq.Region]*pq.RegionWithDistance)

	heap.Push(&toVisit, &pq.RegionWithDistance{R: pq.Region{P: pq.Point{0, 0}, T: pq.Torch}, D: 0})

	var current *pq.RegionWithDistance
	targetRegion := pq.Region{P: target, T: pq.Torch}
	for current == nil || current.R != targetRegion {
		current = heap.Pop(&toVisit).(*pq.RegionWithDistance)
		if _, complete := completeRegions[current.R]; complete {
			continue
		}
		ns := neighbors(depth, *current, target, completeRegions, erosionCache)
		for _, n := range ns {
			s := current.D + travelCost(current.R, n)
			if rd, seen := regionMap[n]; seen && s < rd.D {
				toVisit.Update(rd, s)
			} else {
				rd := pq.RegionWithDistance{R: n, D: s}
				heap.Push(&toVisit, &rd)
				regionMap[n] = &rd
			}
		}
		completeRegions[current.R] = current.D
	}

	return completeRegions[pq.Region{P: target, T: pq.Torch}]
}

func travelCost(src, dest pq.Region) int {
	if src.P == dest.P {
		return 7
	}
	return 1
}

func neighbors(depth int, current pq.RegionWithDistance, target pq.Point, visited map[pq.Region]int, erosionCache map[pq.Point]int) []pq.Region {
	ns := make([]pq.Region, 0, 8)
	var tools []pq.Tool

	//Add current point with the other allowed tool as a neighbor
	tools = toolsAllowed(current.R.P, target, depth, erosionCache)
	var otherTool pq.Tool
	if current.R.T == tools[0] {
		otherTool = tools[1]
	} else {
		otherTool = tools[0]
	}
	region := pq.Region{P: current.R.P, T: otherTool}
	if _, v := visited[region]; !v {
		ns = append(ns, region)
	}

	//Add the adjacent cells that allow current tool as neighbors
	for _, n := range []pq.Point{current.R.P.North(), current.R.P.South(), current.R.P.East(), current.R.P.West()} {
		if n.X >= 0 && n.Y >= 0 {
			tools = toolsAllowed(n, target, depth, erosionCache)
			allowed := tools[0] == current.R.T || tools[1] == current.R.T
			r := pq.Region{P: n, T: current.R.T}
			if _, v := visited[r]; !v && allowed {
				ns = append(ns, r)
			}
		}
	}
	return ns
}

func toolsAllowed(p, target pq.Point, depth int, erosionCache map[pq.Point]int) []pq.Tool {
	switch regionType(depth, p, target, erosionCache) {
	case pq.Rocky:
		return []pq.Tool{pq.Torch, pq.Climbing}
	case pq.Wet:
		return []pq.Tool{pq.Climbing, pq.Neither}
	case pq.Narrow:
		return []pq.Tool{pq.Neither, pq.Torch}
	default:
		panic("Invalid region type")
	}
}

func regionRisk(depth int, region, target pq.Point, erosionCache map[pq.Point]int) int {
	return int(regionType(depth, region, target, erosionCache))
}

func regionType(depth int, region, target pq.Point, erosionCache map[pq.Point]int) pq.RType {
	switch regionErosion(depth, region, target, erosionCache) % 3 {
	case 0:
		return pq.Rocky
	case 1:
		return pq.Wet
	case 2:
		return pq.Narrow
	default:
		panic("invalid region type detected")
	}
}

func regionErosion(depth int, region, target pq.Point, erosionCache map[pq.Point]int) int {
	if i, exists := erosionCache[region]; exists {
		return i
	}
	erosion := (regionGeologicIndex(depth, region, target, erosionCache) + depth) % 20183
	erosionCache[region] = erosion
	return erosion
}

func regionGeologicIndex(depth int, region, target pq.Point, erosionCache map[pq.Point]int) int {
	switch {
	case region.X == 0 && region.Y == 0, region == target:
		return 0
	case region.Y == 0:
		return region.X * 16807
	case region.X == 0:
		return region.Y * 48271
	default:
		return regionErosion(depth, pq.Point{region.X - 1, region.Y}, target, erosionCache) *
			regionErosion(depth, pq.Point{region.X, region.Y - 1}, target, erosionCache)
	}
}
