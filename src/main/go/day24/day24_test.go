package main

import (
	"testing"
	"reflect"
)

func TestParse(t *testing.T) {
	is, inf := parse("test_input.txt")
	expIs := []group {
		{units: 17, hp: 5390, atk: 4507, initiative: 2, atkType: "fire", weaknesses: []string{"radiation", "bludgeoning"}},
		{units: 989, hp: 1274, atk: 25, initiative: 3, atkType: "slashing", weaknesses: []string{"bludgeoning", "slashing"}, immunities: []string{"fire"}},
	}
	expInf := []group {
		{units: 801, hp: 4706, atk: 116, initiative: 1, atkType: "bludgeoning", weaknesses: []string{"radiation"}},
		{units: 4485, hp: 2961, atk: 12, initiative: 4, atkType: "slashing", weaknesses: []string{"fire", "cold"}, immunities: []string{"radiation"}},
	}
	if !reflect.DeepEqual(*is, expIs) {
		t.Errorf("Expected immune system army to be %v but was %v", expIs, *is)
	}
	if !reflect.DeepEqual(*inf, expInf) {
		t.Errorf("Expected infection army to be %v but was %v", expInf, *inf)
	}
}
