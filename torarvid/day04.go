package main

import "fmt"

func day04GetChar(lines []string, x, y int) byte {
	if x < 0 || x >= len(lines[0]) || y < 0 || y >= len(lines) {
		return '?'
	}
	return lines[y][x]
}

func day04FindXmas(lines []string, x, y int) int {
	target := "MAS"
	find := func(xinc, yinc int) bool {
		matchesLeft := 3
		for inc := range 3 {
			char := day04GetChar(lines, x+xinc*(inc+1), y+yinc*(inc+1))
			goal := target[inc]
			if char == goal {
				matchesLeft--
			} else {
				return false
			}
		}
		return matchesLeft == 0
	}
	incs := [][]int{{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}}
	count := 0
	for _, inc := range incs {
		if find(inc[0], inc[1]) {
			count++
		}
	}
	return count
}

func day04FindMas(lines []string, x, y int) bool {
	nw := day04GetChar(lines, x-1, y-1)
	ne := day04GetChar(lines, x+1, y-1)
	sw := day04GetChar(lines, x-1, y+1)
	se := day04GetChar(lines, x+1, y+1)
	matches := 0
	if nw == 'M' && se == 'S' || nw == 'S' && se == 'M' {
		matches++
	}
	if ne == 'M' && sw == 'S' || ne == 'S' && sw == 'M' {
		matches++
	}
	return matches == 2
}

func (adv Advent) Day04() {
	lines := fileByLines("inputs/day04.txt")
	count := 0
	for y, line := range lines {
		for x, char := range line {
			if char == 'X' {
				count += day04FindXmas(lines, x, y)
			}
		}
	}
	fmt.Printf("Part 1: %d\n", count)

	count = 0
	for y, line := range lines {
		for x, char := range line {
			if char == 'A' {
				if day04FindMas(lines, x, y) {
					count++
				}
			}
		}
	}
	fmt.Printf("Part 2: %d\n", count)
}
