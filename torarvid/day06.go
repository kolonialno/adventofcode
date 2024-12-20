package main

import (
	"fmt"
	"strings"
)

type Board [][]bool

type Guard struct {
	Pos [2]int
	Dir int
}

func (g Guard) Copy() Guard {
	return Guard{Pos: [2]int{g.Pos[0], g.Pos[1]}, Dir: g.Dir}
}

var guardChars string = "^>v<"
var dirs [][2]int = [][2]int{{0, -1}, {1, 0}, {0, 1}, {-1, 0}}

func day06CopyBoard(board Board) Board {
	newBoard := make([][]bool, len(board))
	for y, line := range board {
		newBoard[y] = make([]bool, len(line))
		for x := range line {
			newBoard[y][x] = board[y][x]
		}
	}
	return newBoard
}

func day06IsLoop(board Board, guard Guard) bool {
	visitDir := make(map[[3]int]bool)
	for {
		for range 4 {
			dir := dirs[guard.Dir]
			x := guard.Pos[0] + dir[0]
			y := guard.Pos[1] + dir[1]
			if x < 0 || x >= len(board[0]) || y < 0 || y >= len(board) || !board[y][x] {
				break
			}
			guard.Dir = (guard.Dir + 1) % 4
		}
		guard.Pos[0] += dirs[guard.Dir][0]
		guard.Pos[1] += dirs[guard.Dir][1]
		if guard.Pos[0] < 0 || guard.Pos[0] >= len(board[0]) || guard.Pos[1] < 0 || guard.Pos[1] >= len(board) {
			return false
		}
		key := [3]int{guard.Pos[0], guard.Pos[1], guard.Dir}
		if visitDir[key] {
			return true
		}
		visitDir[key] = true
	}
}

func (adv Advent) Day06() {
	lines := fileByLines("inputs/day06.txt")

	board := Board(make([][]bool, len(lines)))
	guard := Guard{Pos: [2]int{0, 0}, Dir: 0}
	for y, line := range lines {
		board[y] = make([]bool, len(line))
		for x, char := range line {
			wall := char == '#'
			board[y][x] = wall
			if !wall && char != '.' {
				guard.Pos = [2]int{x, y}
				guard.Dir = strings.Index(guardChars, string(char))
			}
		}
	}
	origGuard := guard

	visited := make(map[[2]int]bool)
	visited[guard.Pos] = true
	for {
		for range 4 {
			dir := dirs[guard.Dir]
			x := guard.Pos[0] + dir[0]
			y := guard.Pos[1] + dir[1]
			if x < 0 || x >= len(board[0]) || y < 0 || y >= len(board) || !board[y][x] {
				break
			}
			guard.Dir = (guard.Dir + 1) % 4
		}
		guard.Pos[0] += dirs[guard.Dir][0]
		guard.Pos[1] += dirs[guard.Dir][1]
		if guard.Pos[0] < 0 || guard.Pos[0] >= len(board[0]) || guard.Pos[1] < 0 || guard.Pos[1] >= len(board) {
			break
		}
		visited[guard.Pos] = true
	}
	fmt.Println("Part 1:", len(visited))

	obstacleCount := 0
	for pos := range visited {
		x, y := pos[0], pos[1]
		if board[y][x] || y == origGuard.Pos[1] && x == origGuard.Pos[0] {
			continue
		}
		boardCopy := day06CopyBoard(board)
		boardCopy[y][x] = true
		if day06IsLoop(boardCopy, origGuard.Copy()) {
			obstacleCount++
		}
	}
	fmt.Println("Part 2:", obstacleCount)
}
