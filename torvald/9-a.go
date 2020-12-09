package main

import (
	"fmt"
	utils "torvald/libs"
)

func main() {
	numbers, _ := utils.ReadLinesNumbers("input/9.txt")

	preamble := 25

	for i, number := range numbers[preamble:] {
		index := i + preamble
		validNumbers := make(map[int]bool)
		for x := index - preamble; x < index+preamble; x++ {
			for y := index - preamble; y < index+preamble; y++ {
				validNumbers[numbers[x]+numbers[y]] = true
			}
		}
		if _, ok := validNumbers[number]; !ok {
			fmt.Println(number)
			break
		}
	}

}
