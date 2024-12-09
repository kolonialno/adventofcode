package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

type Op func(int, int) int

func day07Calc(terms []int, target int, ops []Op) bool {
	numOps := len(ops)
	limit := int(math.Pow(float64(numOps), float64(len(terms)-1)))
	for i := 0; i < limit; i++ {
		acc := terms[0]
		for j := 1; j < len(terms); j++ {
			idiv := i
			for k := j; k > 1; k-- {
				idiv /= numOps
			}
			acc = ops[idiv%numOps](acc, terms[j])
		}
		if acc == target {
			return true
		}
	}
	return false
}

func (adv Advent) Day07() {
	lines := fileByLines("inputs/day07.txt")
	sum1, sum2 := 0, 0

	add := func(a, b int) int { return a + b }
	mul := func(a, b int) int { return a * b }
	concat := func(a, b int) int {
		strTerm := strconv.Itoa(b)
		for k := 0; k < len(strTerm); k++ {
			a *= 10
			a += int(strTerm[k] - '0')
		}
		return a
	}

	part1Ops := []Op{add, mul}
	part2Ops := []Op{add, mul, concat}

	for _, line := range lines {
		parts := strings.Split(line, ": ")
		target := Atoi[int](parts[0])
		stringTerms := strings.Split(parts[1], " ")
		terms := Aatoii[int](stringTerms...)
		if day07Calc(terms, target, part1Ops) {
			sum1 += target
		}
		if day07Calc(terms, target, part2Ops) {
			sum2 += target
		}
	}

	fmt.Printf("Part 1: %d\n", sum1)
	fmt.Printf("Part 2: %d\n", sum2)
}
