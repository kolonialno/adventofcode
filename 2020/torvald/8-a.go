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

	instructionsSeen := make(map[int]int)
	accumulator := 0

	for i := 0; i < len(instructions); i++ {

		if _, exists := instructionsSeen[i]; exists {
			fmt.Println(accumulator)
			break
		}

		switch instructions[i].kind {
		case "acc":
			accumulator += instructions[i].value
		case "jmp":
			i += instructions[i].value - 1
		}

		instructionsSeen[i]++
	}
}
