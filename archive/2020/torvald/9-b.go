package main

import (
	"fmt"
	utils "torvald/libs"
)

func main() {
	numbers, _ := utils.ReadLinesNumbers("input/9.txt")

	preamble := 25

	find := func(numbers []int) int {
		for i, number := range numbers[preamble:] {
			index := i + preamble
			validNumbers := make(map[int]bool)
			for x := index - preamble; x < index+preamble; x++ {
				for y := index - preamble; y < index+preamble; y++ {
					validNumbers[numbers[x]+numbers[y]] = true
				}
			}
			if _, ok := validNumbers[number]; !ok {
				fmt.Println("not valid ", number)
				return number
			}
		}
		return 0
	}

	invalidNumber := find(numbers)

	for i, number := range numbers {
		x, low, high := number, number, 0
		j := i
		for x < invalidNumber {
			j++
			if numbers[j] > high {
				high = numbers[j]
			}
			x += numbers[j]
			if x == invalidNumber {
				fmt.Println("Sucess at index", i, ":", low+high)
				return
			}
		}
	}
}
