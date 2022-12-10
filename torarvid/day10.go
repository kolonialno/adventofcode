package main

import (
	"fmt"
	"strings"
)

func day10Draw(pixel, X int, frameBuf []byte) {
	cmp := pixel % 40
	if cmp == X-1 || cmp == X || cmp == X+1 {
		frameBuf[pixel] = '#'
	} else {
		frameBuf[pixel] = ' '
	}
}

func day10Solve(lines []string) (int, []byte) {
	X := 1
	cycle := 1
	sigStrength := 0
	frameBuf := make([]byte, 40*6)
	for _, line := range lines {
		diff := 0
		cycles := 1
		if strings.HasPrefix(line, "addx") {
			fmt.Sscanf(line, "addx %d", &diff)
			cycles = 2
		}
		for i := 0; i < cycles; i++ {
			day10Draw(cycle-1, X, frameBuf)
			cycle++
			if i == 1 {
				X += diff
			}
			if cycle <= 220 && (cycle-20)%40 == 0 {
				sigStrength += X * cycle
			}
		}
	}
	return sigStrength, frameBuf
}

func (a Advent) Day10() {
	lines := fileByLines("inputs/day10.txt")

	sigStrength, frameBuf := day10Solve(lines)
	fmt.Println("Part 1:", sigStrength)
	fmt.Println("Part 2:")
	for i := 0; i < 6; i++ {
		fmt.Println(string(frameBuf[i*40 : (i+1)*40]))
	}
}
