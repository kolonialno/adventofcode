package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"os"
	"sort"
	"strings"
)

type Field struct {
	key   string
	value string
}

func importInput() []string {
	file, err := os.Open("5.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")

	return s
}

func binarySearch(value string) int {
	left := 0
	right := 127
	colLeft := 0
	colRight := 7

	seatID := 0

	for _, val := range value {

		mid := (float64(left+right) / 2.0)
		colMid := (float64(colLeft+colRight) / 2.0)

		if string(val) == "F" {
			right = int(math.Floor(mid))
		} else if string(val) == "B" {
			left = int(math.Ceil(mid))
		} else if string(val) == "R" {
			colLeft = int(math.Ceil(colMid))
		} else if (string(val)) == "L" {
			colRight = int(math.Floor(colMid))
		}
		seatID = left*8 + colLeft
	}

	return seatID

}

func getHighestSeatID(boardingPasses []string) int {

	highest := 0

	for _, val := range boardingPasses {
		binarySearchResult := binarySearch(val)
		if binarySearchResult > highest {
			highest = binarySearchResult
		}
	}

	return highest

}

func getMissingSeats(boardingPasses []string) int {

	missingSeats := []int{}
	for _, val := range boardingPasses {
		binarySearchResult := binarySearch(val)
		missingSeats = append(missingSeats, binarySearchResult)
	}
	sort.Ints(missingSeats)
	i := missingSeats[1]
	for i < len(missingSeats)-1 {
		if missingSeats[i+1] != missingSeats[i]+1 {
			return missingSeats[i] + 1
		}
		if missingSeats[i-1] != missingSeats[i]-1 {
			return missingSeats[i] - 1
		}
		i++
	}

	return 0

}

func main() {
	input := importInput()
	result := getHighestSeatID(input)
	fmt.Println(result)
	result2 := getMissingSeats(input)
	fmt.Println(result2)
}
