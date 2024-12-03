package main

import (
	"fmt"
	"strings"
)

func day02IsSafe(numbers []int) bool {
	delta := numbers[1] - numbers[0]
	adelta := abs(delta)
	// fmt.Printf("numbers %v\n", numbers)
	if adelta > 3 || adelta < 1 {
		// fmt.Printf("adelta1 %v\n", adelta)
		return false
	}
	safe := true
	for i := range numbers[2:] {
		d := numbers[i+2] - numbers[i+1]
		ad := abs(d)
		if ad > 3 || ad < 1 {
			// fmt.Printf("adelta2 %v\n", adelta)
			return false
		}
		if (d / ad) != (delta / adelta) {
			// fmt.Printf("d/ad %v, delta/adelta %v\n", d, delta)
			return false
		}
		// fmt.Printf("safe: d %v, ad %v, delta %v, adelta %v\n", d, ad, delta, adelta)
	}
	return safe
}

func (adv Advent) Day02() {
	lines := fileByLines("inputs/day02.txt")
	safeCount := 0
	allNums := make([][]int, len(lines))
	for i, line := range lines {
		parts := strings.Split(line, " ")
		numbers := make([]int, len(parts))
		allNums[i] = numbers
		for j, part := range parts {
			numbers[j] = atoi[int](part)
		}
		if day02IsSafe(numbers) {
			safeCount++
		}
	}
	fmt.Printf("Part 1: %d\n", safeCount)

	safeCount = 0
	for _, numbers := range allNums {
		if day02IsSafe(numbers) {
			safeCount++
			// fmt.Println("  yep")
		} else {
			for i := range len(numbers) {
				// slice of numbers excluding the ith element
				slice := make([]int, 0)
				slice = append(slice, numbers[:i]...)
				slice = append(slice, numbers[i+1:]...)
				if day02IsSafe(slice) {
					safeCount++
					// fmt.Println("  yep")
					break
				}
			}
		}
	}
	fmt.Printf("Part 2: %d\n", safeCount)
}
