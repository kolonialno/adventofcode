package main

import (
	"bufio"
	"fmt"
	"os"
)

type answer struct {
	partOne int
	partTwo int
}

func priority(char rune) int {

	A := int('A')
	a := int('a')

	intChar := int(char)

	if intChar >= a {
		return intChar - a + 1
	}

	return intChar - A + 27
}

func scoreRoundOne(line string) int {

	// Go doesn't have sets, using a map of struct{} (which is 0 bytes)
	leftComparment := make(map[rune]struct{})
	var exists = struct{}{}

	for _, v := range line[0 : len(line)/2] {
		leftComparment[v] = exists
	}

	totalPriority := 0
	for _, v := range line[len(line)/2:] {

		if _, found := leftComparment[v]; found {
			totalPriority += priority(v)
			delete(leftComparment, v)
		}
	}

	return totalPriority
}

func scoreRoundTwo(lines []string) int {

	var exists = struct{}{}

	possibleItems0 := make(map[rune]struct{})
	for _, v := range lines[0] {
		possibleItems0[v] = exists
	}

	possibleItems1 := make(map[rune]struct{})
	for _, v := range lines[1] {
		if _, found := possibleItems0[v]; found {
			possibleItems1[v] = exists
		}
	}

	for _, v := range lines[2] {
		if _, found := possibleItems1[v]; found {
			return priority(v)
		}
	}

	return -1

}

func solve(filePath string) answer {

	input, _ := os.Open(filePath)
	defer input.Close()

	scoreRoundOneSum := 0
	scoreRoundTwoSum := 0
	iter := 1
	roundTwoLines := []string{}

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		scoreRoundOneSum += scoreRoundOne(scanner.Text())

		roundTwoLines = append(roundTwoLines, scanner.Text())
		if iter%3 == 0 {
			scoreRoundTwoSum += scoreRoundTwo(roundTwoLines)
			roundTwoLines = []string{}
		}

		iter += 1
	}

	return answer{partOne: scoreRoundOneSum, partTwo: scoreRoundTwoSum}
}

func main() {

	filePath := "day03.txt"
	fmt.Println(solve(filePath))

}
