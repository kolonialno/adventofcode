package main

import (
	"fmt"
	"strings"
)

func (a Advent) Day02() {
	lines := fileByLines("inputs/day02.txt")

	score := 0
	choiceMap := map[string]int{"A": 1, "B": 2, "C": 3, "X": 1, "Y": 2, "Z": 3}
	resultMap := map[int]int{0: 3, 1: 6, 2: 0, -1: 0, -2: 6}
	for _, line := range lines {
		parts := strings.Split(line, " ")
		result := choiceMap[parts[1]] - choiceMap[parts[0]]
		score += choiceMap[parts[1]] + resultMap[result]
	}

	fmt.Println("Part 1:", score)

	score = 0
	rotateMap := map[string]int{"X": 2, "Y": 0, "Z": 1}
	for _, line := range lines {
		parts := strings.Split(line, " ")
		choices := "ABC"
		index := strings.Index(choices, parts[0])
		rotation := rotateMap[parts[1]]
		choices = choices[rotation:] + choices[:rotation]
		choice := string(choices[index])
		result := choiceMap[choice] - choiceMap[parts[0]]
		score += choiceMap[choice] + resultMap[result]
	}

	fmt.Println("Part 2:", score)
}
