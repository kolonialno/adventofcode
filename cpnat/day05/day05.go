package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type answer struct {
	partOne string
	partTwo string
}

const numStacks = 9

func parseInput(filePath string) ([numStacks][]string, [][]int) {

	input, _ := os.Open(filePath)
	defer input.Close()

	var stacks [numStacks][]string
	var moves [][]int
	scanner := bufio.NewScanner(input)

	for scanner.Scan() {

		line := scanner.Text()
		if strings.Contains(string(line), "[") {

			offset := 1
			for i := 0; i < numStacks; i++ {

				if offset > len(line)-1 {
					break
				}

				char := string(line[offset])
				if string(char) != " " {
					newStack := make([]string, 0)
					newStack = append(stacks[i], []string{char}...)
					stacks[i] = newStack
				}

				offset += 4
			}
		}

		if strings.Contains(string(line), "move") {

			move := []int{}
			re := regexp.MustCompile(`\d+`)
			matches := re.FindAllString(line, -1)

			for _, match := range matches {
				num, _ := strconv.Atoi(match)
				move = append(move, num)
			}
			moves = append(moves, move)
		}
	}

	return stacks, moves

}

func moveStacks(stacks [numStacks][]string, moves [][]int, reverse bool) string {

	for i := 0; i < len(moves); i++ {

		amount := moves[i][0]
		from := moves[i][1] - 1
		to := moves[i][2] - 1

		l := make([]string, amount)
		copy(l, stacks[from][0:amount])

		if reverse == true {
			for i, j := 0, len(l)-1; i < j; i, j = i+1, j-1 {
				l[i], l[j] = l[j], l[i]
			}
		}

		stacks[to] = append(l, stacks[to]...)
		stacks[from] = stacks[from][amount:]
	}

	output := ""

	for i := 0; i < len(stacks); i++ {
		output += stacks[i][0]
	}

	return output

}

func solve(filePath string) answer {

	stacks, moves := parseInput(filePath)
	partOne := moveStacks(stacks, moves, true)
	partTwo := moveStacks(stacks, moves, false)

	return answer{partOne: partOne, partTwo: partTwo}

}

func main() {

	filePath := "day05.txt"
	fmt.Println(solve(filePath))

}
