package main

import "fmt"

func (a Advent) Day06() {
	input := fileByLines("inputs/day06.txt")[0]

	solve := func(req int) int {
		for i := req; i < len(input); i++ {
			m := make(map[byte]int)
			maybeSolved := true
			for j := 0; j < req; j++ {
				current := m[input[i-j]]
				if current > 0 {
					maybeSolved = false
					break
				}
				m[input[i-j]] += 1
			}
			if maybeSolved {
				return i + 1
			}
		}
		panic("no solution found")
	}

	fmt.Println("Part 1:", solve(4))
	fmt.Println("Part 2:", solve(14))
}
