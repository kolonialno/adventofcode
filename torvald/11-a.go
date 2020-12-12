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

	calc := func(x int, y int) rune {
		if array[x][y] == floor {
			return floor
		}
		neighbours := 0
		for i := -1; i < 2; i++ {
			for j := -1; j < 2; j++ {
				if x+i < 0 || x+i >= len(array) {
					continue
				}
				if y+j < 0 || y+j >= len(array) {
					continue
				}
				if i == 0 && j == 0 {
					continue
				}
				if array[x+i][y+j] == occupied {
					neighbours++
				}
			}
		}
		if array[x][y] == chair && neighbours == 0 {
			return occupied
		}
		if array[x][y] == occupied && neighbours >= 4 {
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
