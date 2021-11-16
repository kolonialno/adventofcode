package main

import (
	"fmt"
	"log"
	utils "torvald/libs"
)

func main() {

	array, err := utils.ReadRuneMatrix("input/3.txt")

	if err != nil {
		log.Fatalf("readLines: %s", err)
	}

	tree := []rune("#")[0]

	slopes := [][]int{
		{1, 1},
		{3, 1},
		{5, 1},
		{7, 1},
		{1, 2},
	}

	results := make([]int, len(slopes))

	for slopeIndex, slope := range slopes {
		x := 0
		y := 0
		treeCount := 0

		right := slope[0]
		down := slope[1]

		for x < len(array)-1 {
			x = x + down
			y = (y + right) % len(array[x])

			if array[x][y] == tree {
				treeCount = treeCount + 1
			}
		}
		results[slopeIndex] = treeCount
	}

	answer := 1
	for _, treeCount := range results {
		answer = answer * treeCount
	}

	fmt.Println(answer)

}
