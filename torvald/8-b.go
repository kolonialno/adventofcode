package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"
	utils "torvald/libs"
)

type Instruction struct {
	kind  string
	value int
}

func main() {
	lines, err := utils.ReadLinesStrings("input/8.txt")

	instructions := []Instruction{}

	for _, line := range lines {
		split := strings.Split(line, " ")
		value, _ := strconv.Atoi(split[1])
		instructions = append(instructions, Instruction{
			kind:  split[0],
			value: value,
		})
	}

	run := func(instructions []Instruction) bool {
		instructionsSeen := make(map[int]int)
		accumulator := 0
		for i := 0; i < len(instructions); i++ {

			switch instructions[i].kind {
			case "acc":
				accumulator += instructions[i].value
			case "jmp":
				i += instructions[i].value - 1
			}

			if _, exists := instructionsSeen[i]; exists || i < 0 {
				return false
			}

			instructionsSeen[i]++

		}
		// found 'ya
		fmt.Println(accumulator)
		return true
	}

	for i, _ := range instructions {
		instructionsTry := make([]Instruction, len(instructions))
		copy(instructionsTry, instructions)

		switch instructionsTry[i].kind {
		case "nop":
			instructionsTry[i].kind = "jmp"
		case "jmp":
			instructionsTry[i].kind = "nop"
		default:
			continue
		}

		if run(instructionsTry) {
			return
		}
	}

}
