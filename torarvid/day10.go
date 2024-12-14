package main

import "fmt"

type D10Point [2]int

func (p D10Point) Neighbors(max D10Point) []D10Point {
	neighbors := []D10Point{}
	if p[0]-1 >= 0 {
		neighbors = append(neighbors, D10Point{p[0] - 1, p[1]})
	}
	if p[0]+1 <= max[0] {
		neighbors = append(neighbors, D10Point{p[0] + 1, p[1]})
	}
	if p[1]-1 >= 0 {
		neighbors = append(neighbors, D10Point{p[0], p[1] - 1})
	}
	if p[1]+1 <= max[1] {
		neighbors = append(neighbors, D10Point{p[0], p[1] + 1})
	}
	return neighbors
}

type D10Visits map[D10Point]bool

func (v D10Visits) Clone() D10Visits {
	clone := make(D10Visits)
	for k, v := range v {
		clone[k] = v
	}
	return clone
}

type D10Map struct {
	max        D10Point
	topo       map[D10Point]int
	trailheads []D10Point
}

func (m *D10Map) Search() {
	rating := 0
	var pathFinder func(from D10Point, scores map[D10Point]int) int
	pathFinder = func(from D10Point, scores map[D10Point]int) int {
		fromVal := m.topo[from]
		for _, neighbor := range from.Neighbors(m.max) {
			neighborVal := m.topo[neighbor]
			if neighborVal != fromVal+1 {
				continue
			}
			if neighborVal == 9 {
				scores[neighbor] = 1
				rating++
			}
			pathFinder(neighbor, scores)
		}
		sum := 0
		for _, score := range scores {
			sum += score
		}
		return sum
	}
	sum := 0
	for _, trailhead := range m.trailheads {
		visited := make(D10Visits)
		scores := make(map[D10Point]int)
		visited[trailhead] = true
		sum += pathFinder(trailhead, scores)
	}
	fmt.Printf("Part 1: %d\n", sum)
	fmt.Printf("Part 2: %d\n", rating)
}

func (adv Advent) Day10() {
	lines := fileByLines("inputs/day10.txt")
	topo := make(map[D10Point]int)
	trailheads := []D10Point{}
	var max D10Point
	for y, line := range lines {
		for x, char := range line {
			val := int(char) - int('0')
			topo[D10Point{x, y}] = val
			if val == 0 {
				trailheads = append(trailheads, D10Point{x, y})
			}
			max[0] = Max(max[0], x)
		}
		max[1] = y
	}

	m := D10Map{topo: topo, trailheads: trailheads, max: max}
	m.Search()
}
