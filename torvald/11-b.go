package main

import (
	"fmt"
	utils "torvald/libs"
)

func main() {

	array, _ := utils.ReadRuneMatrix("input/11.txt")

	chair := 'L'
	floor := '.'
	occupied := '#'

	type Direction struct {
		x int
		y int
	}

	directions := make([]Direction, 0)

	for i := -1; i < 2; i++ {
		for j := -1; j < 2; j++ {
			if i == 0 && j == 0 {
				continue
			}
			directions = append(directions, Direction{x: i, y: j})
		}
	}

	calc := func(x int, y int) rune {
		if array[x][y] == floor {
			return floor
		}
		neighbours := 0
		for _, direction := range directions {
			walk := true
			i, j := x, y
			for walk {
				i, j = i+direction.x, j+direction.y
				if i < 0 || i >= len(array) ||
					j < 0 || j >= len(array) ||
					array[i][j] == chair {
					walk = false
					continue
				}
				if array[i][j] == occupied {
					neighbours++
					walk = false
				}
			}
		}
		if array[x][y] == chair && neighbours == 0 {
			return occupied
		}
		if array[x][y] == occupied && neighbours >= 5 {
			return chair
		}
		return array[x][y]
	}

	round := func() ([][]rune, int) {
		checksum := 0
		nextRound := make([][]rune, len(array))
		for x := range array {
			nextRound[x] = make([]rune, len(array[x]))
			for y := range array[x] {
				nextRound[x][y] = calc(x, y)
				checksum += int(array[x][y]) * x * y
			}
		}
		return nextRound, checksum
	}

	printLayout := func(array [][]rune) {
		occupiedSeats := 0
		for x := range array {
			for y := range array[x] {
				fmt.Print(string(array[x][y]))
				if array[x][y] == occupied {
					occupiedSeats++
				}
			}
			fmt.Println("")
		}
		fmt.Println("------", occupiedSeats, "seats ------")
	}

	checksum, newChecksum := 0, 0

	for true {
		array, newChecksum = round()
		if checksum == newChecksum {
			printLayout(array)
			break
		}
		checksum = newChecksum
	}

}
