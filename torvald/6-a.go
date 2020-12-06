package main

import (
	"fmt"
	utils "torvald/libs"
)

type Group struct {
	UniqeQuestions map[rune]bool
}

func newGroup() Group {
	return Group{UniqeQuestions: make(map[rune]bool)}
}

func main() {
	lines, _ := utils.ReadLinesStrings("input/6.txt")

	count := 0

	groups := make([]Group, 0)
	g := newGroup()

	for _, line := range lines {

		if line == "" {
			count += len(g.UniqeQuestions)
			groups = append(groups, g)
			g = newGroup()
			continue
		}

		for _, q := range []rune(line) {
			g.UniqeQuestions[q] = true
		}
	}

	fmt.Println(count)
}
