package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

type Step struct {
	right int
	down  int
}

func importInput() []string {
	file, err := os.Open("3.txt")

	if err != nil {
		log.Fatal(err)
	}

	b, err := ioutil.ReadAll(file)

	s := strings.Split(string(b), "\n")
	inputList := make([]string, 0, len(s))

	for _, val := range s {
		inputList = append(inputList, val)
	}
	return inputList
}

func followPath(grid []string, step Step) int {
	trees := 0
	xIndex := 0

	columnLen := len(grid[0])

	yIndex := 0

	for yIndex < len(grid) {
		row := grid[yIndex]
		a := string(row[xIndex])
		if a == "#" {
			trees++
		}
		xIndex = (xIndex + step.right) % columnLen
		yIndex = (yIndex + step.down)
	}

	return trees
}

func productOfPaths(grid []string) int {
	steps := [5]Step{{right: 1, down: 1}, {right: 3, down: 1}, {right: 5, down: 1}, {right: 7, down: 1}, {right: 1, down: 2}}
	product := 1

	for _, step := range steps {
		product = product * followPath(grid, step)
	}
	return product
}

func main() {
	grid := importInput()
	fmt.Println(grid)
	result := followPath(grid, Step{right: 3, down: 1})
	fmt.Println(result)
	result2 := productOfPaths(grid)
	fmt.Println(result2)
}
