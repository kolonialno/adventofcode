package main

import (
	"fmt"
	utils "torvald/libs"
)

type Group struct {
	Questions map[rune]int
	People    int
}

func newGroup() Group {
	return Group{
		Questions: make(map[rune]int),
		People:    0,
	}
}

func main() {
	lines, _ := utils.ReadLinesStrings("input/6.txt")

	count := 0

	groups := make([]Group, 0)
	g := newGroup()

	for _, line := range lines {

		if line == "" {
			for _, questions := range g.Questions {
				if questions == g.People {
					count += 1
				}
			}
			groups = append(groups, g)
			g = newGroup()
			continue
		}

		g.People += 1

		for _, q := range []rune(line) {
			_, exists := g.Questions[q]
			if exists {
				g.Questions[q] += 1
			} else {
				g.Questions[q] = 1
			}
		}

	}

	fmt.Println(count)
}
