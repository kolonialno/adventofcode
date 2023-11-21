package main

import (
	"fmt"
)

func (a Advent) Day04() {
	lines := fileByLines("inputs/day04.txt")
	score1, score2 := 0, 0
	for _, line := range lines {
		var aStart, aEnd, bStart, bEnd int
		fmt.Sscanf(line, "%d-%d,%d-%d", &aStart, &aEnd, &bStart, &bEnd)

		// part 1
		if aStart >= bStart && aEnd <= bEnd {
			score1 += 1
		} else if bStart >= aStart && bEnd <= aEnd {
			score1 += 1
		}

		// part 2
		if aStart >= bStart && aStart <= bEnd {
			score2 += 1
		} else if bStart >= aStart && bStart <= aEnd {
			score2 += 1
		}
	}
	fmt.Println("Part 1:", score1)
	fmt.Println("Part 2:", score2)
}
