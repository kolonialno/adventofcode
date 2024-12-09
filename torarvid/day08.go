package main

import "fmt"

type D8Loc [2]int
type D8Antennas map[byte][]D8Loc
type D8Pair [2]D8Loc

func (p D8Pair) Antinodes(max D8Loc) []D8Loc {
	dx, dy := p[0][0]-p[1][0], p[0][1]-p[1][1]
	a1x := p[0][0] + dx
	a1y := p[0][1] + dy
	a2x := p[1][0] - dx
	a2y := p[1][1] - dy

	result := make([]D8Loc, 0, 2)
	if a1x >= 0 && a1x < max[0] && a1y >= 0 && a1y < max[1] {
		result = append(result, D8Loc{a1x, a1y})
	}
	if a2x >= 0 && a2x < max[0] && a2y >= 0 && a2y < max[1] {
		result = append(result, D8Loc{a2x, a2y})
	}
	return result
}

func (p D8Pair) Antinodes2(max D8Loc) map[D8Loc]bool {
	nodes := make(map[D8Loc]bool)
	var inner func(D8Loc, D8Loc)
	inner = func(c1, c2 D8Loc) {
		if nodes[c1] && nodes[c2] {
			return
		}
		nodes[c1] = true
		nodes[c2] = true
		dx, dy := c1[0]-c2[0], c1[1]-c2[1]
		a1x := c1[0] + dx
		a1y := c1[1] + dy
		a2x := c2[0] - dx
		a2y := c2[1] - dy
		if a1x >= 0 && a1x < max[0] && a1y >= 0 && a1y < max[1] {
			inner(D8Loc{a1x, a1y}, c1)
		}
		if a2x >= 0 && a2x < max[0] && a2y >= 0 && a2y < max[1] {
			inner(c2, D8Loc{a2x, a2y})
		}
	}
	inner(p[0], p[1])
	return nodes
}

func D8Pairs(locs []D8Loc) []D8Pair {
	if len(locs) < 2 {
		return []D8Pair{}
	}
	pairs := D8Pairs(locs[1:])
	for _, loc := range locs[1:] {
		pairs = append(pairs, D8Pair{locs[0], loc})
	}
	return pairs
}

func (adv Advent) Day08() {
	lines := fileByLines("inputs/day08.txt")
	m := make(D8Antennas)

	for y, line := range lines {
		for x, char := range line {
			if char == '.' {
				continue
			}
			m[byte(char)] = append(m[byte(char)], D8Loc{x, y})
		}
	}
	max := D8Loc{len(lines[0]), len(lines)}
	antinodes := make(map[D8Loc]bool)
	for c, locs := range m {
		_ = c
		pairs := D8Pairs(locs)
		for _, pair := range pairs {
			for _, loc := range pair.Antinodes(max) {
				antinodes[loc] = true
			}
		}
	}
	fmt.Printf("Part 1: %d\n", len(antinodes))
	antinodes = make(map[D8Loc]bool)
	for _, locs := range m {
		pairs := D8Pairs(locs)
		for _, pair := range pairs {
			for loc := range pair.Antinodes2(max) {
				antinodes[loc] = true
			}
		}
	}
	fmt.Printf("Part 2: %d\n", len(antinodes))
}
