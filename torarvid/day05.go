package main

import "fmt"

func (a Advent) Day05() {
	lines := fileByLines("inputs/day05.txt")

	sectionSep := 0
	var stacks []string
	for i, line := range lines {
		sectionSep = i
		if line == "" {
			break
		}
		for j := 1; j < len(line); j += 4 {
			ch := line[j : j+1]
			if ch == " " {
				continue
			}
			if ch == "1" {
				break
			}
			index := (j - 1) / 4
			for len(stacks) <= index {
				stacks = append(stacks, "")
			}
			stacks[index] += ch
		}
	}

	moveCrates := func(input []string, crateMover9001 bool) string {
		output := make([]string, len(input))
		copy(output, input)

		for _, line := range lines[sectionSep+1:] {
			var count, from, to int
			fmt.Sscanf(line, "move %d from %d to %d", &count, &from, &to)
			from, to = from-1, to-1
			var increment int
			if crateMover9001 {
				increment = count
			} else {
				increment = 1
			}
			for i := 0; i < count; i += increment {
				ch := output[from][:increment]
				output[from] = output[from][increment:]
				output[to] = ch + output[to]
			}
		}

		answer := ""
		for _, s := range output {
			answer += s[:1]
		}
		return answer
	}

	fmt.Println("Part 1:", moveCrates(stacks, false))
	fmt.Println("Part 2:", moveCrates(stacks, true))
}
