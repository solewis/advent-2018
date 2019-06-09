package main

import (
	"advent-2018/src/main/go/day22/pq"
	"testing"
)

func TestSample(t *testing.T) {
	risk := calculateRisk(510, pq.Point{X: 10, Y: 10})
	expectedRisk := 114
	if risk != expectedRisk {
		t.Errorf("Expected risk %d but was %d", expectedRisk, risk)
	}
}

func TestSampleShortestPath(t *testing.T) {
	sp := calculateShortestPath(510, pq.Point{X: 10, Y: 10})
	ex := 45
	if sp != ex {
		t.Errorf("Expected shortest path %d but was %d", ex, sp)
	}
}
