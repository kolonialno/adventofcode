package main

import (
	"fmt"
	_ "regexp"
	"strconv"
	utils "torvald/libs"
)

func main() {
	lines, _ := utils.ReadLinesStrings("input/12.txt")

	x, y := 10, 1
	xShip, yShip := 0, 0

	trans := func(rotation int, x int, y int) (int, int) {
		if rotation == -1 {
			return -y, x
		}
		if rotation == -2 {
			return -x, -y
		}
		if rotation == -3 {
			return y, -x
		}
		if rotation == 1 {
			return y, -x
		}
		if rotation == 2 {
			return -x, -y
		}
		if rotation == 3 {
			return -y, x
		}
		return 0, 0

	}

	for _, line := range lines {
		cmd := []rune(line[:1])[0]
		value, _ := strconv.Atoi(line[1:])
		if cmd == 'R' {
			x, y = trans(value/90, x, y)
		}
		if cmd == 'L' {
			x, y = trans(-value/90, x, y)
		}
		if cmd == 'F' {
			xShip, yShip = (x*value)+xShip, (y*value)+yShip
		}
		if cmd == 'E' {
			x += value
		}
		if cmd == 'S' {
			y -= value
		}
		if cmd == 'W' {
			x -= value
		}
		if cmd == 'N' {
			y += value
		}
	}

	fmt.Println(utils.IntAbs(xShip) + utils.IntAbs(yShip))
}
