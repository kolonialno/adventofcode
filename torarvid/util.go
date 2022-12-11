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

func abs[T constraints.Signed | constraints.Float](a T) T {
	if a < 0 {
		return -a
	}
	return a
}

func chebyshevDist[T constraints.Signed | constraints.Float](a, b []T) T {
	var dist T
	for i := range a {
		dist = max(dist, abs(a[i]-b[i]))
	}
	return dist
}

func atoi[T constraints.Integer](s string) T {
	i, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		panic(err)
	}
	return T(i)
}

func Map[T, U any](a []T, f func(T) U) []U {
	b := make([]U, len(a))
	for i, x := range a {
		b[i] = f(x)
	}
	return b
}
