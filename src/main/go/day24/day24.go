package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

//type attackType string
//
//const (
//	cold        attackType = "cold"
//	bludgeoning            = "bludgeoning"
//	slashing               = "slashing"
//	fire                   = "fire"
//	radiation              = "radiation"
//)

type group struct {
	units, hp, atk, initiative int
	atkType                    string
	weaknesses, immunities     []string
}

type army []*group

func parse(filename string) (immuneSystem, infection army) {
	dat, err := ioutil.ReadFile(filename)
	check(err)
	lines := strings.Split(string(dat), "\n")

	is := make([]group, 0)
	inf := make([]group, 0)
	re := regexp.MustCompile(`(\d+) units each with (\d+) hit points (?:\((.+)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)`)
	//immune to radiation; weak to fire, cold
	var current *[]group
	for _, l := range lines {
		fmt.Println(l)
		switch l {
		case "Immune System:":
			current = &is
			continue
		case "":
			continue
		case "Infection:":
			current = &inf
			continue
		}

		matches := re.FindStringSubmatch(l)
		units, _ := strconv.Atoi(matches[1])
		hp, _ := strconv.Atoi(matches[2])
		atk, _ := strconv.Atoi(matches[4])
		atkType := matches[5]
		initiative, _ := strconv.Atoi(matches[6])
		*current = append(*current, group{
			units: units,
			hp: hp,
			atk: atk,
			initiative: initiative,
			atkType: atkType,
		})
		fmt.Println(matches)
	}
	return
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
