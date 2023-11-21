package main

import (
	"fmt"
	"sort"
	"strconv"
)

func (a Advent) Day01() {
	lines := fileByLines("inputs/day01.txt")
	index := 0
	sums := make([]int, len(lines))
	for _, line := range lines {
		if line == "" {
			index += 1
			continue
		}
		number, _ := strconv.Atoi(line)
		sums[index] += number
	}

	sort.Ints(sums)

	fmt.Printf("Part 1: %d\n", sums[len(sums)-1])
	fmt.Printf("Part 2: %d\n", sums[len(sums)-1]+sums[len(sums)-2]+sums[len(sums)-3])
}
