package main

import (
	"testing"
)

func TestSampleStrongestInRange(t *testing.T) {
	nanobots := parse("test_input.txt")

	strongest := strongest(nanobots)
	expectedStrongest := nanobot{coordinate{0, 0, 0}, 4}
	if strongest != expectedStrongest {
		t.Errorf("Expected strongest: %v but was %v", expectedStrongest, strongest)
	}

	ct := inRangeCount(strongest, nanobots)
	expectedCt := 7
	if ct != expectedCt {
		t.Errorf("Expected %d nanobots in range of strongest, but was %d", expectedCt, ct)
	}
}

func TestParseHandlesNegatives(t *testing.T) {
	nanobots := parse("test_input_negatives.txt")
	if len(nanobots) != 1 {
		t.Error("Expected 1 nanobots, but was:", len(nanobots))
	}
	expected := nanobot{coordinate{-1, -1, -1}, 1}
	if nanobots[0] != expected {
		t.Errorf("Expected nanobot %v but was %v", expected, nanobots[0])
	}
}

func TestCubeSplit2x(t *testing.T) {
	c := cube{coordinate{0,0,0}, 2}
	cubes := c.split()
	expected := []cube{
		{coordinate{0,1,1}, 1},
		{coordinate{0,1,0}, 1},
		{coordinate{0,0,1}, 1},
		{coordinate{0,0,0}, 1},
		{coordinate{1,1,1}, 1},
		{coordinate{1,1,0}, 1},
		{coordinate{1,0,1}, 1},
		{coordinate{1,0,0}, 1},
	}
	if !sameCubes(cubes, expected) {
		t.Errorf("Expected cubes to be split to %v but was split to %v", expected, cubes)
	}
}

func TestCubeSplit4x(t *testing.T) {
	c := cube{coordinate{0,0,0}, 4}
	cubes := c.split()
	expected := []cube{
		{coordinate{0,2,2}, 2},
		{coordinate{0,2,0}, 2},
		{coordinate{0,0,2}, 2},
		{coordinate{0,0,0}, 2},
		{coordinate{2,2,2}, 2},
		{coordinate{2,2,0}, 2},
		{coordinate{2,0,2}, 2},
		{coordinate{2,0,0}, 2},
	}
	if !sameCubes(cubes, expected) {
		t.Errorf("Expected cubes to be split to %v but was split to %v", expected, cubes)
	}
}

func TestCubeDistanceAway(t *testing.T) {
	c := cube{min: coordinate{0,0,0}, length: 1}
	if c.distanceAway(coordinate{0, 0, 1}) != 1 {
		t.Errorf("Z distance failed")
	}
	if c.distanceAway(coordinate{0, 1, 0}) != 1 {
		t.Errorf("Y distance failed")
	}
	if c.distanceAway(coordinate{1, 0, 0}) != 1 {
		t.Errorf("X distance failed")
	}
	if c.distanceAway(coordinate{1, 1, 1}) != 3 {
		t.Errorf("All distance failed")
	}
	if c.distanceAway(coordinate{-1, -1, -1}) != 3 {
		t.Errorf("All distance failed negative")
	}
}

func TestSampleTeleportPoint(t *testing.T) {
	nanobots := parse("test_input_teleport.txt")
	tp := teleportPoint(nanobots)
	expected := coordinate{12, 12, 12}
	if tp.min != expected {
		t.Errorf("Expected coordinate %v but was %v", expected, tp)
	}
}

func sameCubes(x, y []cube) bool {
	xMap := make(map[cube]int)
	yMap := make(map[cube]int)

	for _, xElem := range x {
		xMap[xElem]++
	}
	for _, yElem := range y {
		yMap[yElem]++
	}

	for xMapKey, xMapVal := range xMap {
		if yMap[xMapKey] != xMapVal {
			return false
		}
	}
	return true
}
