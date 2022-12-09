package main

import "fmt"

type day9Knot struct {
	row, col int
}

func (k day9Knot) ToArr() []int {
	return []int{k.row, k.col}
}

func (k *day9Knot) TailMove(knotInFront day9Knot) {
	if chebyshevDist(k.ToArr(), knotInFront.ToArr()) > 1 {
		rowDiff := knotInFront.row - k.row
		if rowDiff != 0 {
			rowDiff /= abs(rowDiff)
		}
		colDiff := knotInFront.col - k.col
		if colDiff != 0 {
			colDiff /= abs(colDiff)
		}
		k.row, k.col = k.row+rowDiff, k.col+colDiff
	}
}

type day9Knots []day9Knot

func day9NewKnots(numKnots int) day9Knots {
	knots := make(day9Knots, numKnots)
	for i := range knots {
		knots[i] = day9Knot{}
	}
	return knots
}

func (knots day9Knots) solve(lines []string) int {
	visits := make(map[day9Knot]int)
	visits[knots[len(knots)-1]] = 1

	for _, line := range lines {
		var dir string
		var dist int
		fmt.Sscanf(line, "%s %d", &dir, &dist)
		for i := 0; i < dist; i++ {
			switch dir {
			case "U":
				knots[0].row -= 1
			case "D":
				knots[0].row += 1
			case "L":
				knots[0].col -= 1
			case "R":
				knots[0].col += 1
			}
			for j := 1; j < len(knots); j++ {
				knots[j].TailMove(knots[j-1])
			}
			visits[knots[len(knots)-1]] = 1
		}
	}
	return len(visits)
}

func (a Advent) Day09() {
	lines := fileByLines("inputs/day09.txt")
	fmt.Println("Part 1:", day9NewKnots(2).solve(lines))
	fmt.Println("Part 2:", day9NewKnots(10).solve(lines))
}
