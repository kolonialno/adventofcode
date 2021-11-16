package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

func importInput() []string {
	file, err := os.Open("11.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	return s
}

func applyRules(seatMap []string) []string {
	newSeatMap := make([]string, 0, len(seatMap))
	for i, row := range seatMap {
		newRow := []rune{}
		for j, char := range row {
			adjacentOccupied := 0
			if j > 0 && row[j-1] == '#' {
				adjacentOccupied++
			}
			if j+1 < len(row) && row[j+1] == '#' {
				adjacentOccupied++
			}
			if i > 0 && seatMap[i-1][j] == '#' {
				adjacentOccupied++
			}
			if i+1 < len(seatMap) && seatMap[i+1][j] == '#' {
				adjacentOccupied++
			}
			if j > 0 && i > 0 && seatMap[i-1][j-1] == '#' {
				adjacentOccupied++
			}
			if j > 0 && i+1 < len(seatMap) && seatMap[i+1][j-1] == '#' {
				adjacentOccupied++
			}
			if j+1 < len(row) && i > 0 && seatMap[i-1][j+1] == '#' {
				adjacentOccupied++
			}
			if j+1 < len(row) && i+1 < len(seatMap) && seatMap[i+1][j+1] == '#' {
				adjacentOccupied++
			}
			if char == 'L' && adjacentOccupied == 0 {
				newRow = append(newRow, '#')
			} else if char == '#' && adjacentOccupied >= 4 {
				newRow = append(newRow, 'L')
			} else {
				newRow = append(newRow, char)
			}
		}
		newSeatMap = append(newSeatMap, string(newRow))
	}
	return newSeatMap
}

func mapsEqual(seatMap []string, newSeatMap []string) bool {
	if len(seatMap) != len(newSeatMap) {
		return false
	}

	for i := 0; i < len(seatMap); i++ {
		if len(seatMap[i]) != len(newSeatMap[i]) {
			return false
		}

		for j := 0; j < len(seatMap[i]); j++ {
			if seatMap[i][j] != newSeatMap[i][j] {
				return false
			}
		}
	}
	return true
}

func countOccupied(seatMap []string) int {

	occupied := 0
	for _, row := range seatMap {
		for _, char := range row {
			if char == '#' {
				occupied++
			}
		}
	}
	return occupied
}

func partOne(seatMap []string) int {

	for true {
		newSeatMap := applyRules(seatMap)
		if mapsEqual(seatMap, newSeatMap) {
			break
		}
		seatMap = newSeatMap
	}
	return countOccupied(seatMap)
}

func isOccupied(seatMap []string, col int, row int, colDirection int, rowDirection int) int {
	newCol := col + colDirection
	newRow := row + rowDirection

	if newRow >= len(seatMap) {
		return 0
	}
	if newRow < 0 {
		return 0
	}
	if newCol >= len(seatMap[newRow]) {
		return 0
	}
	if newCol < 0 {
		return 0
	}
	if seatMap[newRow][newCol] == 'L' {
		return 0
	}

	if seatMap[newRow][newCol] == '#' {
		return 1
	}

	return isOccupied(seatMap, newCol, newRow, colDirection, rowDirection)
}

func partTwo(seatMap []string) int {

	for true {

		newSeatMap := make([]string, 0, len(seatMap))
		for rowIndex, row := range seatMap {
			newRow := []rune{}
			for colIndex, char := range row {
				if char == '.' {
					newRow = append(newRow, char)
					continue
				}
				directions := [][]int{[]int{0, 1}, []int{1, 0}, []int{0, -1}, []int{-1, 0}, []int{1, 1}, []int{1, -1}, []int{-1, 1}, []int{-1, -1}}
				adjacentOccupied := 0
				for _, direction := range directions {
					adjacentOccupied += isOccupied(seatMap, colIndex, rowIndex, direction[0], direction[1])
				}

				if char == 'L' && adjacentOccupied == 0 {
					newRow = append(newRow, '#')
				} else if char == '#' && adjacentOccupied >= 5 {
					newRow = append(newRow, 'L')
				} else {
					newRow = append(newRow, char)
				}
			}
			newSeatMap = append(newSeatMap, string(newRow))
		}
		if mapsEqual(seatMap, newSeatMap) {
			break
		}
		seatMap = newSeatMap
	}
	return countOccupied(seatMap)
}

func main() {
	input := importInput()
	result := partOne(input)
	fmt.Println(result)
	result2 := partTwo(input)
	fmt.Println(result2)
}
