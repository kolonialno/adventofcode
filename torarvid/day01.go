package main

import (
	"fmt"
	"slices"
	"strconv"
	"strings"
)

func (adv Advent) Day01() {
	lines := fileByLines("inputs/day01.txt")
	a := make([]int, len(lines))
	b := make([]int, len(lines))
	counts := make(map[int]int)
	for i, line := range lines {
		firstSpace := strings.Index(line, " ")
		x, _ := strconv.Atoi(strings.TrimSpace(line[:firstSpace]))
		y, _ := strconv.Atoi(strings.TrimSpace(line[firstSpace+1:]))

		a[i], b[i] = x, y
		counts[y] = counts[y] + 1
	}
	slices.Sort(a)
	slices.Sort(b)
	sum := 0
	for i := range a {
		sum += abs(a[i] - b[i])
	}
	fmt.Printf("Part 1: %d\n", sum)

	similarityScore := 0
	for i := range a {
		similarityScore += a[i] * counts[a[i]]
	}
	fmt.Printf("Part 2: %d\n", similarityScore)
}
