package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"strconv"
	"strings"
)

type instruction struct {
	op      string
	a, b, c int
}

func (i instruction) string(registers []int) string {
	switch i.op {
	case "addr":
		return fmt.Sprintf("r[%d] = %d", i.c, registers[i.a] + registers[i.b])
	case "addi":
		return fmt.Sprintf("r[%d] = %d", i.c, registers[i.a] + i.b)
	case "mulr":
		return fmt.Sprintf("r[%d] = %d", i.c, registers[i.a] * registers[i.b])
	case "muli":
		return fmt.Sprintf("r[%d] = %d", i.c, registers[i.a] * i.b)
	case "banr":
		return fmt.Sprintf("r[%d] = %d", i.c, registers[i.a] & registers[i.b])
	case "bani":
		return fmt.Sprintf("r[%d] = %d", i.c, registers[i.a] & i.b)
	case "borr":
		return fmt.Sprintf("r[%d] = %d", i.c, registers[i.a] | registers[i.b])
	case "bori":
		return fmt.Sprintf("r[%d] = %d", i.c, registers[i.a] | i.b)
	case "setr":
		return fmt.Sprintf("r[%d] = %d", i.c, registers[i.a])
	case "seti":
		return fmt.Sprintf("r[%d] = %d", i.c, i.a)
	case "gtir":
		return fmt.Sprintf("r[%d] = if %d > %d 1 else 0", i.c, i.a, registers[i.b])
	case "gtri":
		return fmt.Sprintf("r[%d] = if %d > %d 1 else 0", i.c, registers[i.a], i.b)
	case "gtrr":
		return fmt.Sprintf("r[%d] = if %d > %d 1 else 0", i.c, registers[i.a], registers[i.b])
	case "eqir":
		return fmt.Sprintf("r[%d] = if %d == %d 1 else 0", i.c, i.a, registers[i.b])
	case "eqri":
		return fmt.Sprintf("r[%d] = if %d == %d 1 else 0", i.c, registers[i.a], i.b)
	case "eqrr":
		return fmt.Sprintf("r[%d] = if %d == %d 1 else 0", i.c, registers[i.a], registers[i.b])
	default:
		return ""
	}
}

func main() {
	fmt.Println(math.MaxInt64)
	boundRegister, instructions := parse("src/main/go/day19/input.txt")
	ip := 0
	registers := []int{1, 0, 0, 0, 0, 0}
	for ip < len(instructions) && ip >= 0 {
		//update bound register to value of instruction pointer
		registers[boundRegister] = ip
		//run instruction
		fmt.Println(instructions[ip].string(registers))
		execute(instructions[ip], &registers)
		//update instruction pointer to value of bound register and add 1
		ip = registers[boundRegister]
		ip++
		//fmt.Println(registers)
	}
	fmt.Println("Part 1:", registers[0])
}

func execute(ins instruction, registers *[]int) {
	switch ins.op {
	case "addr":
		(*registers)[ins.c] = (*registers)[ins.a] + (*registers)[ins.b]
	case "addi":
		(*registers)[ins.c] = (*registers)[ins.a] + ins.b
	case "mulr":
		(*registers)[ins.c] = (*registers)[ins.a] * (*registers)[ins.b]
	case "muli":
		(*registers)[ins.c] = (*registers)[ins.a] * ins.b
	case "banr":
		(*registers)[ins.c] = (*registers)[ins.a] & (*registers)[ins.b]
	case "bani":
		(*registers)[ins.c] = (*registers)[ins.a] & ins.b
	case "borr":
		(*registers)[ins.c] = (*registers)[ins.a] | (*registers)[ins.b]
	case "bori":
		(*registers)[ins.c] = (*registers)[ins.a] | ins.b
	case "setr":
		(*registers)[ins.c] = (*registers)[ins.a]
	case "seti":
		(*registers)[ins.c] = ins.a
	case "gtir":
		if ins.a > (*registers)[ins.b] {
			(*registers)[ins.c] = 1
		} else {
			(*registers)[ins.c] = 0
		}
	case "gtri":
		if (*registers)[ins.a] > ins.b {
			(*registers)[ins.c] = 1
		} else {
			(*registers)[ins.c] = 0
		}
	case "gtrr":
		if (*registers)[ins.a] > (*registers)[ins.b] {
			(*registers)[ins.c] = 1
		} else {
			(*registers)[ins.c] = 0
		}
	case "eqir":
		if ins.a == (*registers)[ins.b] {
			(*registers)[ins.c] = 1
		} else {
			(*registers)[ins.c] = 0
		}
	case "eqri":
		if (*registers)[ins.a] == ins.b {
			(*registers)[ins.c] = 1
		} else {
			(*registers)[ins.c] = 0
		}
	case "eqrr":
		if (*registers)[ins.a] == (*registers)[ins.b] {
			(*registers)[ins.c] = 1
		} else {
			(*registers)[ins.c] = 0
		}
	}
}

func parse(filename string) (ip int, instructions []instruction) {
	dat, err := ioutil.ReadFile(filename)
	check(err)
	lines := strings.Split(string(dat), "\n")
	ipLine, insLines := lines[0], lines[1:]
	ip, _ = strconv.Atoi(ipLine[len(ipLine)-1:])

	instructions = make([]instruction, len(insLines))
	for i, l := range insLines {
		sections := strings.Split(l, " ")
		a, _ := strconv.Atoi(sections[1])
		b, _ := strconv.Atoi(sections[2])
		c, _ := strconv.Atoi(sections[3])
		instructions[i] = instruction{op: sections[0], a: a, b: b, c: c}
	}
	return ip, instructions
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
