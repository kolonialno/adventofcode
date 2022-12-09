package main

import (
	"bufio"
	"os"
	"strconv"

	"golang.org/x/exp/constraints"
)

func fileByLines(filename string) []string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	return lines
}

func fileByIntLines(filename string) []int {
	lines := fileByLines(filename)
	var ints []int
	for _, line := range lines {
		i, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		ints = append(ints, i)
	}
	return ints
}

func max[T constraints.Ordered](a, b T) T {
	if a > b {
		return a
	}
	return b
}

func abs[T constraints.Signed](a T) T {
	if a < 0 {
		return -a
	}
	return a
}
