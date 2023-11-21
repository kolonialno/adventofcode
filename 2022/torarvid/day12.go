package main

import "fmt"

type day12Node struct{ X, Y int }
type day12Edge struct{ From, To day12Node }
type day12Graph map[day12Node][]day12Edge

var start, target day12Node

func day12NewGraph(lines []string) day12Graph {
	g := make(day12Graph)
	heights := make(map[[2]int]int)
	for y, line := range lines {
		for x, c := range line {
			height := int(c - 'a')
			if c == 'S' {
				height = 0
				start = day12Node{x, y}
			}
			if c == 'E' {
				height = int('z' - 'a')
				target = day12Node{x, y}
			}
			heights[[2]int{x, y}] = height
			g[day12Node{x, y}] = nil
		}
	}
	for y := 0; y < len(lines); y++ {
		for x := 0; x < len(lines[y]); x++ {
			for _, offset := range [][2]int{{1, 0}, {0, 1}, {-1, 0}, {0, -1}} {
				dx, dy := offset[0], offset[1]
				heightDiff := heights[[2]int{x + dx, y + dy}] - heights[[2]int{x, y}]
				n := day12Node{x + dx, y + dy}
				if heightDiff <= 1 {
					g[n] = append(g[n], day12Edge{n, day12Node{x, y}})
				}
			}
		}
	}
	return g
}

func (g day12Graph) ShortestDistance(from day12Node) map[day12Node]int {
	visited := make(map[day12Node]bool)
	steps := make(map[day12Node]int)
	for node := range g {
		steps[node] = 1e9
	}
	steps[from] = 0
	queue := []day12Node{from}
	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]
		for _, edge := range g[node] {
			dest, newSteps := edge.To, steps[node]+1
			if !visited[dest] && newSteps < steps[dest] {
				queue = append(queue, dest)
				steps[dest] = newSteps
			}
		}
		visited[node] = true
	}
	return steps
}

func (a Advent) Day12() {
	lines := fileByLines("inputs/day12.txt")
	g := day12NewGraph(lines)
	steps := g.ShortestDistance(target)
	fmt.Println("Part 1:", steps[start])

	shortest := int(1e9)
	for y, line := range lines {
		for x, c := range line {
			if c == 'S' || c == 'a' {
				shortest = min(shortest, steps[day12Node{x, y}])
			}
		}
	}
	fmt.Println("Part 2:", shortest)
}
