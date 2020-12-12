package main

import (
	"fmt"
	_ "log"
	_ "regexp"
	"strconv"
	utils "torvald/libs"
)

func main() {
	lines, _ := utils.ReadLinesStrings("input/12.txt")

	directions := []rune{'E', 'S', 'W', 'N'}
	direction := 0

	x, y := 0, 0

	for _, line := range lines {
		cmd := []rune(line[:1])[0]
		value, _ := strconv.Atoi(line[1:])
		// fmt.Println(cmd, value)
		if cmd == 'R' {
			direction = utils.ModLikePython((direction + (value / 90)), len(directions))
		}
		if cmd == 'L' {
			direction = utils.ModLikePython((direction - (value / 90)), len(directions))
		}
		if cmd == 'F' {
			if directions[direction] == 'E' {
				x += value
			}
			if directions[direction] == 'S' {
				y += value
			}
			if directions[direction] == 'W' {
				x -= value
			}
			if directions[direction] == 'N' {
				y -= value
			}
		}
		if cmd == 'E' {
			x += value
		}
		if cmd == 'S' {
			y += value
		}
		if cmd == 'W' {
			x -= value
		}
		if cmd == 'N' {
			y -= value
		}

	}

	fmt.Println(x + y)
}
