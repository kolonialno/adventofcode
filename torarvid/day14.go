package main

import (
	"fmt"
	"strings"
)

const (
	day14Air = iota
	day14Rock
	day14Sand
)

type day14Cave map[[2]int]int

func (c day14Cave) Play(part2 bool) int {
	xMin, xMax, yMin, yMax := int(1e9), int(-1e9), int(0), int(-1e9)
	for coord := range c {
		if coord[0] < xMin {
			xMin = coord[0]
		}
		if coord[0] > xMax {
			xMax = coord[0]
		}
		if coord[1] < yMin {
			yMin = coord[1]
		}
		if coord[1] > yMax {
			yMax = coord[1]
		}
	}
	if part2 {
		for x := xMin - 500; x <= xMax+500; x++ {
			c[[2]int{x, yMax + 2}] = day14Rock
		}
		xMin, xMax, yMax = xMin-500, xMax+500, yMax+2
	}
	sandCount := 0
	for {
		prevSandCount := sandCount
		sand := [2]int{500, 0}
		for {
			old := sand
			for _, x := range []int{0, -1, 1} {
				candidate := [2]int{sand[0] + x, sand[1] + 1}
				if c[candidate] == day14Air {
					sand[0] = candidate[0]
					sand[1] = candidate[1]
					break
				}
			}
			if sand[0] < xMin || sand[0] > xMax || sand[1] > yMax {
				break
			}
			if old == sand {
				if c[sand] == day14Air {
					c[sand] = day14Sand
					sandCount++
				}
				break
			}
		}
		if sandCount == prevSandCount {
			return sandCount
		}
	}
}

func (a Advent) Day14() {
	lines := fileByLines("inputs/day14.txt")

	cave := day14Cave{}
	for _, line := range lines {
		coords := strings.Split(line, " -> ")
		for i := 1; i < len(coords); i++ {
			from := strings.Split(coords[i-1], ",")
			to := strings.Split(coords[i], ",")
			xf, yf := atoi[int](from[0]), atoi[int](from[1])
			xt, yt := atoi[int](to[0]), atoi[int](to[1])
			for x := min(xf, xt); x <= max(xf, xt); x++ {
				for y := min(yf, yt); y <= max(yf, yt); y++ {
					cave[[2]int{x, y}] = day14Rock
				}
			}
		}
	}
	fmt.Println("Part 1:", cave.Play(false))

	for k, v := range cave {
		if v == day14Sand {
			cave[k] = day14Air
		}
	}

	fmt.Println("Part 2:", cave.Play(true))
}
