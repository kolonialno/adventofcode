package main

import (
	"fmt"
	"strings"
)

func (a Advent) Day03() {
	lines := fileByLines("inputs/day03.txt")
	addScore := func(char byte) int {
		val := char - 'a' + 1
		if val > 26 {
			val -= 198
		}
		return int(val)
	}
	score := 0
	for _, line := range lines {
		sep := len(line) / 2
		first, second := line[:sep], line[sep:]
		for _, char := range first {
			if strings.Contains(second, string(char)) {
				score += addScore(byte(char))
				break
			}
		}
	}

	fmt.Println("Part 1:", score)

	score = 0
	for i := 0; i < len(lines); i += 3 {
		chunk := lines[i : i+3]
		for _, char := range chunk[0] {
			if strings.Contains(chunk[1], string(char)) &&
				strings.Contains(chunk[2], string(char)) {
				score += addScore(byte(char))
				break
			}
		}
	}

	fmt.Println("Part 2:", score)
}
