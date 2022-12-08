package main

import "fmt"

type day8Grid [][]int

func (g day8Grid) Above(row, col int) []int {
	a := make([]int, row)
	for otherRow := row - 1; otherRow >= 0; otherRow-- {
		a[row-otherRow-1] = g[otherRow][col]
	}
	return a
}

func (g day8Grid) Below(row, col int) []int {
	a := make([]int, len(g)-row-1)
	for otherRow := row + 1; otherRow < len(g); otherRow++ {
		a[otherRow-row-1] = g[otherRow][col]
	}
	return a
}

func (g day8Grid) Left(row, col int) []int {
	a := make([]int, col)
	for otherCol := col - 1; otherCol >= 0; otherCol-- {
		a[col-otherCol-1] = g[row][otherCol]
	}
	return a
}

func (g day8Grid) Right(row, col int) []int {
	a := make([]int, len(g[row])-col-1)
	for otherCol := col + 1; otherCol < len(g[row]); otherCol++ {
		a[otherCol-col-1] = g[row][otherCol]
	}
	return a
}

func (a Advent) Day08() {
	lines := fileByLines("inputs/day08.txt")

	grid := make(day8Grid, len(lines))
	for i, line := range lines {
		grid[i] = make([]int, len(line))
		for j, c := range line {
			grid[i][j] = int(c - '0')
		}
	}

	visibleInnerTrees := 0
	for row := 1; row < len(grid)-1; row++ {
		for col := 1; col < len(grid[row])-1; col++ {
			tree := grid[row][col]
			directions := []func(int, int) []int{grid.Above, grid.Below, grid.Left, grid.Right}
			for _, direction := range directions {
				candidate := true
				for _, otherTree := range direction(row, col) {
					if otherTree >= tree {
						candidate = false
						break
					}
				}
				if candidate {
					visibleInnerTrees++
					break
				}
			}
		}
	}
	visibleOuterTrees := len(lines)*2 + len(lines[0])*2 - 4
	fmt.Println("Part 1:", visibleInnerTrees+visibleOuterTrees)

	maxScenicScore := 0
	for row := 1; row < len(grid)-1; row++ {
		for col := 1; col < len(grid[row])-1; col++ {
			tree := grid[row][col]
			directions := []func(int, int) []int{grid.Above, grid.Below, grid.Left, grid.Right}
			scenicScore := 1
			for _, direction := range directions {
				directionScore := 0
				for _, otherTree := range direction(row, col) {
					directionScore++
					if otherTree >= tree {
						break
					}
				}
				scenicScore *= directionScore
			}
			if scenicScore > maxScenicScore {
				maxScenicScore = scenicScore
			}
		}
	}
	fmt.Println("Part 2:", maxScenicScore)
}
