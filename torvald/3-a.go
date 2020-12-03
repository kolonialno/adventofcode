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

	treeCount := 0
	x := 0
	y := 0

	for x < len(array)-1 {
		x = x + 1
		y = (y + 3) % len(array[x])

		if array[x][y] == tree {
			treeCount = treeCount + 1
		}
	}

	fmt.Println(treeCount)
}
