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

func solve(filePath string) answer {

	input, _ := os.Open(filePath)
	defer input.Close()

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
	}

	return answer{partOne: -1, partTwo: -1}

}

func main() {

	filePath := "day00.txt"
	fmt.Println(solve(filePath))

}
