package main

import (
	"fmt"
	"slices"
	"strings"
)

func (adv Advent) Day05() {
	lines := fileByLines("inputs/day05.txt")

	orders := make(map[int][]int)
	nums := make(map[int]bool)

	firstSectionMarker := 0
	for i, line := range lines {
		if line == "" {
			firstSectionMarker = i
			break
		}
		parts := strings.Split(line, "|")
		a, b := Atoi[int](parts[0]), Atoi[int](parts[1])
		orders[a] = append(orders[a], b)
		nums[a], nums[b] = true, true
	}
	sorter := func(a, b int) int {
		na, aexists := orders[a]
		if aexists && slices.Index(na, b) >= 0 {
			return -1
		}
		nb, bexists := orders[b]
		if bexists && slices.Index(nb, a) >= 0 {
			return 1
		}
		return 0
	}

	sum1, sum2 := 0, 0
	for _, line := range lines[firstSectionMarker+1:] {
		parts := strings.Split(line, ",")
		nums := Aatoii[int](parts...)
		if slices.IsSortedFunc(nums, sorter) {
			sum1 += nums[len(nums)/2]
		} else {
			slices.SortFunc(nums, sorter)
			sum2 += nums[len(nums)/2]
		}
	}
	fmt.Printf("Part 1: %d\n", sum1)
	fmt.Printf("Part 2: %d\n", sum2)
}
