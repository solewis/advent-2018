package main

import (
	"testing"
)

func TestSimplePath(t *testing.T) {
	d := p1("ENWW")
	if d != 4 {
		t.Error("Longest path should have been 4, but was", d)
	}
}

func TestChooseLongerBranch(t *testing.T) {
	d := p1("ENWW(S|NN)")
	if d != 6 {
		t.Error("Longest path should have been 6, but was", d)
	}
}

func TestNestedBranches(t *testing.T) {
	d := p1("ENWWW(NEEE|SSE(EE|N))")
	if d != 10 {
		t.Error("Longest path should have been 10, but was", d)
	}
}

func TestBranchAsBeginningOfBranch(t *testing.T) {
	d := p1("NE(NN(EN(W|NN)|WW)|S)")
	if d != 8 {
		t.Error("Longest path should have been 8, but was", d)
	}
}

func TestDetoursAsLongest(t *testing.T) {
	d := p1("ENNWSWW(NEWS|)S")
	if d != 9 {
		t.Error("Longest path should have been 9, but was", d)
	}
}

func TestDetoursAsIgnored(t *testing.T) {
	d := p1("ENNWSWW(NEWS|)SSSEEN")
	if d != 13 {
		t.Error("Longest path should have been 13, but was", d)
	}
}

func TestDetoursAtEnd(t *testing.T) {
	d := p1("ENNWSWW(NEWS|)")
	if d != 9 {
		t.Error("Longest path should have been 9, but was", d)
	}
}

func TestExample2(t *testing.T) {
	d := p1("ENWWW(NEEE|SSE(EE|N))")
	if d != 10 {
		t.Error("Longest path should have been 10, but was", d)
	}
}

func TestExample3(t *testing.T) {
	d := p1("ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN")
	if d != 18 {
		t.Error("Longest path should have been 18, but was", d)
	}
}

func TestExample4(t *testing.T) {
	d := p1("ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))")
	if d != 23 {
		t.Error("Longest path should have been 23, but was", d)
	}
}

func TestExample5(t *testing.T) {
	d := p1("WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))")
	if d != 31 {
		t.Error("Longest path should have been 31, but was", d)
	}
}

func TestCount(t *testing.T) {
	limit := 6
	c:= p2("ENWW(S|NN)", limit)
	expected:=1
	if c != expected {
		t.Errorf("paths equal or over %d should be %d, but was %d", limit, expected, c)
	}
}

func TestCount2(t *testing.T) {
	limit := 4
	c:= p2("NN(WNNSSE|)", limit)
	expected:=2
	if c != expected {
		t.Errorf("paths equal or over %d should be %d, but was %d", limit, expected, c)
	}
}

func TestCount3(t *testing.T) {
	limit := 4
	c:= p2("NN(WNSE|)EE", limit)
	expected:=2
	if c != expected {
		t.Errorf("paths equal or over %d should be %d, but was %d", limit, expected, c)
	}
}

func TestCount4(t *testing.T) {
	limit := 4
	c:= p2("NN(WW|E(N|S))", limit)
	expected:=3
	if c != expected {
		t.Errorf("paths equal or over %d should be %d, but was %d", limit, expected, c)
	}
}

func TestCount5(t *testing.T) {
	limit := 4
	c:= p2("NN(WW(N|S)|EE(NNN|SS))", limit)
	expected:=9
	if c != expected {
		t.Errorf("paths equal or over %d should be %d, but was %d", limit, expected, c)
	}
}


