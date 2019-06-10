package main

import (
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

type army *[]group

func parse(filename string) (immuneSystem, infection army) {
	dat, err := ioutil.ReadFile(filename)
	check(err)
	lines := strings.Split(string(dat), "\n")

	is := make([]group, 0)
	inf := make([]group, 0)
	fullRegex := regexp.MustCompile(`(\d+) units each with (\d+) hit points (?:\((.+)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)`)
	innerRegex := regexp.MustCompile(`(weak|immune) to (.+)`)
	//immune to radiation; weak to fire, cold
	var current *[]group
	for _, l := range lines {
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

		matches := fullRegex.FindStringSubmatch(l)
		units, _ := strconv.Atoi(matches[1])
		hp, _ := strconv.Atoi(matches[2])
		atk, _ := strconv.Atoi(matches[4])
		atkType := matches[5]
		initiative, _ := strconv.Atoi(matches[6])

		weaknesses := make([]string, 0)
		immunities := make([]string, 0)
		for _, wi := range strings.Split(matches[3], ";") {
			inner := innerRegex.FindStringSubmatch(wi)
			switch {
			case inner[1] == "weak":
				weaknesses = append(weaknesses, strings.Split(inner[2], ", ")...)
			case inner[1] == "immune":
				immunities = append(immunities, strings.Split(inner[2], ", ")...)
			}
		}
		*current = append(*current, group{
			units: units,
			hp: hp,
			atk: atk,
			initiative: initiative,
			atkType: atkType,
			weaknesses: weaknesses,
			immunities: immunities,
		})
	}
	return &is, &inf
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
