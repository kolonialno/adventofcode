package main

import (
	"fmt"
	"strings"
)

func (a Advent) Day10() {
	lines := fileByLines("inputs/day10.txt")
	X := 1
	cycle := 1
	sigStrength := 0
	frameBuf := make([]byte, 40*6)

	draw := func(pixel int) {
		cmp := pixel % 40
		frameBuf[pixel] = ' '
		if cmp == X-1 || cmp == X || cmp == X+1 {
			frameBuf[pixel] = '#'
		}
	}

	for _, line := range lines {
		diff := 0
		cycles := 1
		if strings.HasPrefix(line, "addx") {
			fmt.Sscanf(line, "addx %d", &diff)
			cycles = 2
		}
		for i := 0; i < cycles; i++ {
			draw(cycle - 1)
			cycle++
			if i == 1 {
				X += diff
			}
			if cycle <= 220 && (cycle-20)%40 == 0 {
				sigStrength += X * cycle
			}
		}
	}

	fmt.Println("Part 1:", sigStrength)
	fmt.Println("Part 2:")
	for i := 0; i < 6; i++ {
		fmt.Println(string(frameBuf[i*40 : (i+1)*40]))
	}
}
