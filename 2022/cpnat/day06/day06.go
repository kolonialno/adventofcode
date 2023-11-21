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

func getStartOfPacketMarker(line string, offset int) int {

	var exists = struct{}{}

	for i := 0; i < len(line)-offset; i++ {
		currentValues := make(map[byte]struct{})

		for j := i; j < (offset + i); j++ {

			letter := line[j]

			if _, found := currentValues[letter]; found {
				currentValues = make(map[byte]struct{})
			} else {
				currentValues[letter] = exists
			}

			if len(currentValues) == offset {
				return i + offset
			}
		}

	}

	return -1
}

func solve(filePath string) answer {

	input, _ := os.Open(filePath)
	defer input.Close()

	scanner := bufio.NewScanner(input)

	var partOne int
	var partTwo int
	for scanner.Scan() {
		line := scanner.Text()
		partOne = getStartOfPacketMarker(line, 4)
		partTwo = getStartOfPacketMarker(line, 14)
	}

	return answer{partOne: partOne, partTwo: partTwo}

}

func main() {

	filePath := "day06.txt"
	fmt.Println(solve(filePath))

}
